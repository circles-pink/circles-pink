module VoucherServer.MonadApp.Impl.Prod.GraphNodeEnv where

import Prelude

import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (instant)
import Data.Newtype (un)
import Data.Number (fromString)
import Data.Time.Duration (Seconds(..), convertDuration)
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
import VoucherServer.MonadApp.Class (AppError(..), GraphNodeEnv(..), GraphNodeEnv'getTransferMeta)
import VoucherServer.Spec.Types (TransferId(..))
import VoucherServer.Types (TransferMeta(..))

type M a = ReaderT AppEnvVars (ExceptT AppError Aff) a

mkSubgraphUrl :: String -> String -> String
mkSubgraphUrl url subgraphName = url <> "/subgraphs/name/" <> subgraphName

mkClient :: AppEnvVars -> M (Client UrqlClient Schema Unit Unit)
mkClient (AppEnvVars env) =
  GQL.createClient
    { headers: []
    , url: mkSubgraphUrl env.gardenGraphApi env.gardenSubgraphName
    }
    # liftEffect

mkGetTransferMeta :: M (GraphNodeEnv'getTransferMeta AppProdM)
mkGetTransferMeta = do
  appEnvVars@(AppEnvVars env) <- ask

  client :: _ Schema _ _ <- mkClient appEnvVars

  pure \transferId -> do
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

mkGraphNodeEnv :: M (GraphNodeEnv AppProdM)
mkGraphNodeEnv = do
  getTransferMeta <- mkGetTransferMeta
  pure $ GraphNodeEnv
    { getTransferMeta
    }

liftGQL :: forall a. Aff a -> AppProdM a
liftGQL x = try x
  <#> lmap (const ErrGraphQL)
  # liftAff
  >>= liftEither