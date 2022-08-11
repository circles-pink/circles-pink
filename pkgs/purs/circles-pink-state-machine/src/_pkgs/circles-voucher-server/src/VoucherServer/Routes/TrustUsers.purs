module VoucherServer.Routes.TrustUser where

import Prelude

import CirclesCore (Account, CirclesCore)
import CirclesCore as CC
import CirclesCore.Bindings (TrustIsTrustedResult(..))
import CirclesPink.Data.Address (Address)
import Control.Monad.Except (ExceptT, lift, mapExceptT, runExceptT, throwError, withExceptT)
import Convertable (convert)
import Data.Either (Either)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server.Response as Response
import Safe.Coerce (coerce)
import VoucherServer.Env (PrivateKey(..), ServerEnv)
import Web3 (Web3)

type Env =
  { circlesCore :: CirclesCore
  , account :: Account
  , web3 :: Web3
  }

trustUsers :: ServerEnv -> { body :: { safeAddresses :: Array Address } } -> ExceptT (String /\ Failure) Aff {}
trustUsers env { body: { safeAddresses } } = do
  provider <- CC.newWebSocketProvider env.gardenEthereumNodeWebSocket
    # mapExceptT liftEffect
    # withExceptT (\_ -> "Provider error" /\ Error (Response.internalError (StringBody "Internal error")))

  web3 <- liftEffect $ CC.newWeb3 provider

  circlesCore <-
    CC.newCirclesCore web3
      { apiServiceEndpoint: env.gardenApi
      , graphNodeEndpoint: env.gardenGraphApi
      , hubAddress: env.gardenHubAddress
      , proxyFactoryAddress: env.gardenProxyFactoryAddress
      , relayServiceEndpoint: env.gardenRelay
      , safeMasterAddress: env.gardenSafeMasterAddress
      , subgraphName: env.gardenSubgraphName
      , databaseSource: "graph"
      }
      # mapExceptT liftEffect
      # withExceptT (\_ -> "CirclesCore error" /\ Error (Response.internalError (StringBody "Internal error")))

  account <- CC.privKeyToAccount web3 (coerce env.xbgeKey)
    # mapExceptT liftEffect
    # withExceptT (\_ -> "Account creation error" /\ Error (Response.internalError (StringBody "Internal error")))

  results :: Array (Either String Unit) <- for safeAddresses (trustUser env { web3, circlesCore, account } >>> runExceptT) # lift

  logShow results

  pure {}

trustUser :: ServerEnv -> Env -> Address -> ExceptT String Aff Unit
trustUser env { circlesCore, account } safeAddress = do
  (TrustIsTrustedResult { isTrusted }) <-
    CC.trustIsTrusted circlesCore account
      { safeAddress: convert safeAddress, limit: 5 }
      # withExceptT (\_ -> "check is trusted error")

  if isTrusted then
    throwError "Already Trusted"
  else do
    _ <- CC.trustAddConnection circlesCore account
      { user: convert safeAddress
      , canSendTo: convert $ unwrap env.xbgeSafeAddress
      }
      # withExceptT (\_ -> "check is trusted error")
    pure unit
