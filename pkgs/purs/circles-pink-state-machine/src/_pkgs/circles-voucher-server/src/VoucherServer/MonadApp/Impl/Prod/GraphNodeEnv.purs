module VoucherServer.MonadApp.Impl.Prod.GraphNodeEnv where

import Prelude

import CirclesPink.Data.Address (parseAddress)
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.Reader (ask)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut.Decode.Class (class DecodeJsonField)
import Data.Array as A
import Data.BN (fromDecimalStr)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (instant)
import Data.Newtype (un)
import Data.Number (fromString)
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Traversable (for)
import Effect.Aff (Aff, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import GraphQL.Client.Args ((=>>))
import GraphQL.Client.BaseClients.Urql (createClient)
import GraphQL.Client.Query (query)
import GraphQL.Client.Types (class GqlQuery)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.GraphQLSchemas.GraphNode (Schema, selectors)
import VoucherServer.MonadApp (AppProdM)
import VoucherServer.MonadApp.Class (AppError(..), GN'getTransactions, GN'getTransferMeta, GraphNodeEnv(..))
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (MkAppProdM)
import VoucherServer.Spec.Types (Freckles(..), TransferId(..))
import VoucherServer.Specs.Xbge (Address(..))
import VoucherServer.Types (Transfer(..), TransferMeta(..))

type M a = MkAppProdM a

type N a = AppProdM a

mkSubgraphUrl :: String -> String -> String
mkSubgraphUrl url subgraphName = url <> "/subgraphs/name/" <> subgraphName

-- For the stangest reasons this definition must stay top level
-- It should not move inside the socpe of `mkGraphNodeEnv`
-- Otherwise fetched results seem to be cached.
queryGql
  :: forall query returns
   . GqlQuery Schema query returns
  => DecodeJsonField returns
  => DecodeJson returns
  => AppEnvVars
  -> String
  -> query
  -> Aff returns
queryGql (AppEnvVars env) s q =
  do
    (client :: _ Schema _ _) <- liftEffect $ createClient { headers: [], url: (mkSubgraphUrl env.gardenGraphApi env.gardenSubgraphName) }
    query client s q

mkGraphNodeEnv :: M (GraphNodeEnv AppProdM)
mkGraphNodeEnv = do
  { envVars: appEnvVars@(AppEnvVars {xbgeSafeAddress}) } <- ask

  let
    getTransferMeta :: GN'getTransferMeta N
    getTransferMeta transferId = do
      result <-
        let
          { id, transactionHash, time } = selectors
        in
          queryGql appEnvVars "get-transfer-meta"
            { notifications:
                { where:
                    { transfer: un TransferId transferId
                    , safeAddress: show xbgeSafeAddress
                    }
                } =>>
                  { id, transactionHash, time }
            }
            # liftGQL

      { transactionHash, id, time: time' } <- A.head result.notifications
        # liftMaybe (ErrGraphQLParse "Array is empty")

      time <- fromString time' <#> Seconds <#> convertDuration >>= instant
        # liftMaybe (ErrGraphQLParse "Not a number")

      pure $ TransferMeta { time, transactionHash, id }

    getTransactions :: GN'getTransactions N
    getTransactions { toAddress } = do
      { transfers } <-
        let
          { from, to, id, amount } = selectors
        in
          queryGql appEnvVars "get-transactions"
            { transfers:
                { where: { to: show toAddress } } =>>
                  { from, to, id, amount }
            }
            # liftGQL

      for transfers \x -> ado
        from <- Address <$> parseAddress x.from
          # liftMaybe ErrUnknown

        to <- Address <$> parseAddress x.to
          # liftMaybe ErrUnknown

        amount <- Freckles <$> fromDecimalStr x.amount
          # liftMaybe ErrUnknown

        let id = TransferId x.id

        in Transfer { from, to, amount, id }

  pure $ GraphNodeEnv
    { getTransferMeta
    , getTransactions
    }

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

liftGQL :: forall a. Aff a -> AppProdM a
liftGQL x = try x
  <#> lmap (const ErrGraphQL)
  # liftAff
  >>= liftEither