module VoucherServer.Routes.TrustUser where

import Prelude

import CirclesCore (Account, CirclesCore, ErrNewCirclesCore, ErrNewWebSocketProvider)
import CirclesCore as CC
import CirclesCore.Bindings (TrustIsTrustedResult(..))
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.SafeAddress (SafeAddress(..))
import Control.Monad.Except (ExceptT(..), catchError, lift, mapExceptT, throwError, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Convertable (convert)
import Data.Either (Either(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server.Response as Response
import Safe.Coerce (coerce)
import VoucherServer.Env (PrivateKey(..), ServerEnv(..))
import Web3 (Web3)

type Env =
  { circlesCore :: CirclesCore
  , account :: Account
  , web3 :: Web3
  }

data TrustUserResult = AlreadyTrusted | NowTrusted | TrustError

trustUsers :: ServerEnv -> { body :: { safeAddresses :: Array Address } } -> ExceptT (String /\ Failure) Aff (Array TrustUserResult)
trustUsers env { body: { safeAddresses } } = do
  provider <- CC.newWebSocketProvider env.gardenEthereumNodeWebSocket
    # mapExceptT liftEffect
    # withExceptT (\_ -> "" /\ Error (Response.internalError (StringBody "Internal error")))

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
      # withExceptT (\_ -> "" /\ Error (Response.internalError (StringBody "Internal error")))

  account <- CC.privKeyToAccount web3 (coerce env.xbgeKey)
    # mapExceptT liftEffect
    # withExceptT (\_ -> "" /\ Error (Response.internalError (StringBody "Internal error")))

  for safeAddresses $ trustUser env { web3, circlesCore, account }

trustUser :: ServerEnv -> Env -> Address -> ExceptT (String /\ Failure) Aff TrustUserResult
trustUser env { circlesCore, account } safeAddress = do
  (TrustIsTrustedResult { isTrusted }) <-
    CC.trustIsTrusted circlesCore account
      { safeAddress: convert safeAddress, limit: 5 }
      # withExceptT (\_ -> "" /\ Error (Response.internalError (StringBody "Internal error")))

  if isTrusted then
    pure AlreadyTrusted
  else do
    CC.trustAddConnection circlesCore account
      { user: convert safeAddress
      , canSendTo: env.xbgeSafeAddress
      }

  pure TrustError