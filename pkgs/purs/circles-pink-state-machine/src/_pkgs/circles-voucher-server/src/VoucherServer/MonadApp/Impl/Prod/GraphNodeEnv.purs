module VoucherServer.MonadApp.Impl.Prod.GraphNodeEnv where

import Prelude

import CirclesPink.Data.Address (parseAddress)
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.Reader (ask)
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
import GraphQL.Client.BaseClients.Urql (UrqlClient)
import GraphQL.Client.BaseClients.Urql as GQL
import GraphQL.Client.Query (query)
import GraphQL.Client.Types (Client)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.GraphQLSchemas.GraphNode (Schema, selectors)
import VoucherServer.MonadApp (AppProdM)
import VoucherServer.MonadApp.Class (AppError(..), GraphNodeEnv(..), GraphNodeEnv'getTransferMeta, GraphNodeEnv'getTransactions)
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (MkAppProdM)
import VoucherServer.Spec.Types (Freckles(..), TransferId(..))
import VoucherServer.Specs.Xbge (Address(..))
import VoucherServer.Types (Transfer(..), TransferMeta(..))

type M a = MkAppProdM a

mkSubgraphUrl :: String -> String -> String
mkSubgraphUrl url subgraphName = url <> "/subgraphs/name/" <> subgraphName

mkClient :: AppEnvVars -> M (Client UrqlClient Schema Unit Unit)
mkClient (AppEnvVars env) =
  GQL.createClient
    { headers: []
    , url: mkSubgraphUrl env.gardenGraphApi env.gardenSubgraphName
    }
    # liftEffect

mkGraphNodeEnv :: M (GraphNodeEnv AppProdM)
mkGraphNodeEnv = do
  { envVars: appEnvVars@(AppEnvVars env) } <- ask

  client :: _ Schema _ _ <- mkClient appEnvVars

  let
    getTransferMeta :: GraphNodeEnv'getTransferMeta AppProdM
    getTransferMeta transferId = do
      result <-
        let
          { id, transactionHash, time } = selectors
        in
          query client "get-transfer-meta"
            { notifications:
                { where:
                    { transfer: un TransferId transferId
                    , safeAddress: show env.xbgeSafeAddress
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

    getTransactions :: GraphNodeEnv'getTransactions AppProdM
    getTransactions { toAddress } = do
      { transfers } <-
        let
          { from, to, id, amount } = selectors
        in
          query client "get-transactions"
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