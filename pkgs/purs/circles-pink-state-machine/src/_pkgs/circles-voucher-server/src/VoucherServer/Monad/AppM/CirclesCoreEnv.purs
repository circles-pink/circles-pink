module VoucherServer.Monad.AppM.CirclesCoreEnv where

import Prelude

import CirclesCore (Account, CirclesCore, Provider)
import CirclesCore as CC
import CirclesPink.Data.Address (Address(..))
import CirclesPink.Data.Nonce (addressToNonce)
import CirclesPink.Data.PrivateKey (PrivateKey(..))
import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT, withExceptT)
import Control.Monad.Reader (ask)
import Convertable (convert)
import Data.Array as A
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Safe.Coerce (coerce)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.Monad.AppM (AppM)
import VoucherServer.Monad.MkAppM (MkAppM)
import VoucherServer.Types.AppError (AppError(..), CCErrAll)
import VoucherServer.Types.Envs (CC'getPaymentNote, CC'getTrusts, CC'trustAddConnection, CC'trustIsTrusted, CirclesCoreEnv(..), CC'getSafeAddress)
import Web3 (Web3)

type M a = MkAppM a

type N = AppM

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
    getTrusts :: CC'getTrusts N
    getTrusts safeAddress =
      CC.trustGetNetwork circlesCore account
        { safeAddress: convert safeAddress }
        # liftCirclesCore
        <#> A.filter _.isIncoming
          >>> map (_.safeAddress >>> wrap)
          >>> Set.fromFoldable

  let
    getPaymentNote :: CC'getPaymentNote N
    getPaymentNote transactionHash =
      CC.tokenGetPaymentNote circlesCore account
        { transactionHash }
        # liftCirclesCore

  let
    trustAddConnection :: CC'trustAddConnection N
    trustAddConnection opts =
      CC.trustAddConnection circlesCore account
        opts
        # liftCirclesCore

  let
    trustIsTrusted :: CC'trustIsTrusted N
    trustIsTrusted opts =
      CC.trustIsTrusted circlesCore account
        opts
        # liftCirclesCore

  let
    getSafeAddress :: CC'getSafeAddress N
    getSafeAddress addr =
      CC.utilsRequestRelayer circlesCore
        { path: [ "safes", "predict" ]
        , version: 3
        , method: "POST"
        , data:
            { saltNonce: coerce $ addressToNonce $ coerce addr
            , owners: [ convert $ unwrap addr ]
            , threshold: 1
            }
        }
        # liftCirclesCore
        <#> unwrap >>> coerce

  pure $ CirclesCoreEnv
    { getTrusts
    , getPaymentNote
    , trustAddConnection
    , trustIsTrusted
    , getSafeAddress
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
  { envVars : AppEnvVars envVars } <- ask

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
