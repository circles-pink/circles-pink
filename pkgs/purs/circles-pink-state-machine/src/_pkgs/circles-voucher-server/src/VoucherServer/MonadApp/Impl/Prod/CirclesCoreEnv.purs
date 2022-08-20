module VoucherServer.MonadApp.Impl.Prod.CirclesCoreEnv where

import Prelude

import CirclesCore (Account, CirclesCore, Provider)
import CirclesCore as CC
import CirclesPink.Data.PrivateKey (PrivateKey(..))
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Convertable (convert)
import Data.Array as A
import Data.Newtype (wrap)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Safe.Coerce (coerce)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp (AppError(..), AppProdM, CCErrAll)
import VoucherServer.MonadApp.Class (CirclesCoreEnv(..), CirclesCoreEnv'getPaymentNote, CirclesCoreEnv'getTrusts, CirclesCoreEnv'trustAddConnection)
import Web3 (Web3)

type M a = ReaderT AppEnvVars (ExceptT AppError Aff) a

type N = AppProdM

type CirclesValues =
  { provider :: Provider
  , web3 :: Web3
  , circlesCore :: CirclesCore
  , account :: Account
  }

mkCirclesCoreEnv :: M (CirclesCoreEnv N)
mkCirclesCoreEnv = do
  { circlesCore, account } <- getCirclesValues

  let
    getTrusts :: CirclesCoreEnv'getTrusts N
    getTrusts safeAddress =
      CC.trustGetNetwork circlesCore account
        { safeAddress: convert safeAddress }
        # liftCirclesCore
        <#> A.filter _.isOutgoing
          >>> map (_.safeAddress >>> wrap)
          >>> Set.fromFoldable

    getPaymentNote :: CirclesCoreEnv'getPaymentNote N
    getPaymentNote transactionHash =
      CC.tokenGetPaymentNote circlesCore account
        { transactionHash }
        # liftCirclesCore

    trustAddConnection :: CirclesCoreEnv'trustAddConnection N
    trustAddConnection opts = 
      CC.trustAddConnection circlesCore account
        opts
        # liftCirclesCore

  pure $ CirclesCoreEnv
    { getTrusts
    , getPaymentNote
    , trustAddConnection
    }

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

liftCirclesCore :: forall a m. MonadAff m => MonadThrow AppError m => ExceptT CCErrAll Aff a -> m a
liftCirclesCore x = x
  # withExceptT ErrCirclesCore
  # runExceptT
  # liftAff
  >>= liftEither

getCirclesValues :: M CirclesValues
getCirclesValues = do
  AppEnvVars envVars <- ask

  provider <- CC.newWebSocketProvider envVars.gardenEthereumNodeWebSocket
    # mapExceptT liftEffect
    # liftCirclesCore

  web3 <- CC.newWeb3 provider
    # liftEffect
    # liftCirclesCore

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
      # liftCirclesCore

  account <- CC.privKeyToAccount web3 (coerce envVars.xbgeKey)
    # mapExceptT liftEffect
    # liftCirclesCore

  pure { provider, web3, circlesCore, account }
