module VoucherServer.MonadApp.Impl.Prod where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.PrivateKey.Type (PrivateKey(..))
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Convertable (convert)
import Data.Array as A
import Data.Either (Either)
import Data.Newtype (class Newtype, wrap)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Payload.ResponseTypes (Response(..))
import Safe.Coerce (coerce)
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv(..), AppError(..))

newtype AppProdM a = AppProdM
  ( ReaderT (AppEnv AppProdM)
      (ExceptT AppError Aff)
      a
  )

derive instance newtypeAPM ::  Newtype (AppProdM a) _
derive newtype instance applyAPM ::  Apply AppProdM
derive newtype instance applicativeAPM ::  Applicative AppProdM
derive newtype instance functorAPM ::  Functor AppProdM
derive newtype instance bindAPM ::  Bind AppProdM
derive newtype instance monadAPM ::  Monad AppProdM
derive newtype instance monadThrowAPM ::  MonadThrow AppError AppProdM
derive newtype instance monadAskAPM ::  MonadAsk (AppEnv AppProdM) AppProdM
derive newtype instance monadEffectAPM ::  MonadEffect AppProdM
derive newtype instance monadAffAPM ::  MonadAff AppProdM
instance monadVoucherServerAffAPM :: MonadApp AppProdM

mkProdEnv :: AppEnvVars -> ExceptV (CC.Err ()) Aff (AppEnv AppProdM)
mkProdEnv envVars = do
  provider <- CC.newWebSocketProvider envVars.gardenEthereumNodeWebSocket
    # mapExceptT liftEffect

  web3 <- liftEffect $ CC.newWeb3 provider

  circlesCore <-
    CC.newCirclesCore web3
      { apiServiceEndpoint: envVars.gardenApi
      , graphNodeEndpoint: envVars.gardenGraphApi
      , hubAddress: envVars.gardenHubAddress
      , proxyFactoryAddress: envVars.gardenProxyFactoryAddress
      , relayServiceEndpoint: envVars.gardenRelay
      , safeMasterAddress: envVars.gardenSafeMasterAddress
      , subgraphName: envVars.gardenSubgraphName
      , databaseSource: "graph"
      }
      # mapExceptT liftEffect

  account <- CC.privKeyToAccount web3 (coerce envVars.xbgeKey)
    # mapExceptT liftEffect

  pure $ AppEnv
    { envVars

    , getTrusts: \safeAddress ->
        CC.trustGetNetwork circlesCore account
          { safeAddress: convert safeAddress }
          # fromCCAff
          <#> A.filter (_.isOutgoing)
            >>> map (_.safeAddress >>> wrap)
            >>> Set.fromFoldable
    
    }

runAppProdM :: forall a. AppEnv AppProdM -> AppProdM a -> Aff (Either AppError a)
runAppProdM env (AppProdM x) = runExceptT $ runReaderT x env
  

mapResponse :: forall a b. (a -> b) -> Response a -> Response b
mapResponse f (Response r) = Response r { body = f r.body }


--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

fromCCAff :: forall a. ExceptV (CC.Err ()) Aff a -> AppProdM a
fromCCAff x = x
  # withExceptT ErrCirclesCore
  # runExceptT
  # liftAff
  >>= liftEither