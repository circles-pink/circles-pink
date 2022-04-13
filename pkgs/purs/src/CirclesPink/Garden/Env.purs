module CirclesPink.Garden.Env
  ( env, testEnv, EnvVars
  ) where

import Prelude
import CirclesPink.Garden.CirclesCore (CirclesCore, Web3, userResolve)
import CirclesPink.Garden.CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control as C
import CirclesPink.Garden.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT, throwError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Class (liftEffect)
import HTTP (ReqFn)
import Type.Proxy (Proxy(..))
import Wallet.PrivateKey (sampleAddress, zeroKey)
import Wallet.PrivateKey as P

type EnvVars
  = { gardenApi :: String
    , gardenApiUsers :: String
    , gardenGraphApi :: String
    , gardenSubgraphName :: String
    , gardenRelay :: String
    , gardenHubAddress :: String
    , gardenProxyFactoryAddress :: String
    , gardenSafeMasterAddress :: String
    , gardenEthereumNodeWebSocket :: String
    }

_errService :: CirclesError
_errService = inj (Proxy :: _ "errService") unit

_errParse :: CirclesError
_errParse = inj (Proxy :: _ "errParse") unit

env :: { request :: ReqFn (CirclesError' ()), envVars :: EnvVars } -> C.Env Aff
env { request, envVars } =
  { apiCheckUserName
  , apiCheckEmail
  , generatePrivateKey: P.genPrivateKey
  , userRegister:
      \privKey options -> do
        web3 <- mapExceptT liftEffect $ getWeb3 envVars
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        CC.userRegister circlesCore account options
  , getSafeAddress:
      \{ nonce, privKey } -> do
        web3 <- mapExceptT liftEffect $ getWeb3 envVars
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        address <- CC.safePredictAddress circlesCore account { nonce: nonce }
        pure address
  , safePrepareDeploy:
      \{ nonce, privKey } -> do
        web3 <- mapExceptT liftEffect $ getWeb3 envVars
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
        CC.safePrepareDeploy circlesCore account { nonce: nonce }
  , userResolve:
      \{ privKey, safeAddress } -> do
        web3 <- mapExceptT liftEffect $ getWeb3 envVars
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
        users <- userResolve circlesCore account { userNames: [], addresses: [ safeAddress ] }
        case head users of
          Nothing -> throwError (inj (Proxy :: _ "errUserNotFound") { address: safeAddress })
          Just u -> pure u
  }
  where
  apiCheckUserName :: C.EnvApiCheckUserName Aff
  apiCheckUserName username =
    if username == "" then
      pure { isValid: false }
    else
      request
        { url: envVars.gardenApiUsers
        , method: POST
        , body: encodeJson { username }
        }
        # runExceptT
        -- <#> (spy "log")
        
        <#> ( \result -> case result of
              Left e -> Left e
              Right x
                | x.status /= 200 && x.status /= 409 -> Left $ _errService
                | otherwise -> Right x
          )
        <#> ( \result -> do
              res <- result
              body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
              if body'.status == "ok" then
                Right { isValid: true }
              else
                Right { isValid: false }
          )
        # ExceptT

  apiCheckEmail :: C.EnvApiCheckEmail Aff
  apiCheckEmail email =
    if email == "" then
      pure { isValid: false }
    else
      request
        { url: envVars.gardenApiUsers
        , method: POST
        , body: encodeJson { email }
        }
        # runExceptT
        -- <#> (spy "log")
        
        <#> ( \result -> case result of
              Left e -> Left e
              Right x
                | x.status /= 200 && x.status /= 409 -> Left $ _errService
                | otherwise -> Right x
          )
        <#> ( \result -> do
              res <- result
              body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
              if body'.status == "ok" then
                Right { isValid: true }
              else
                Right { isValid: false }
          )
        # ExceptT

getWeb3 :: forall r. EnvVars -> ExceptV ( errNative :: Error | r ) Effect Web3
getWeb3 ev = do
  provider <- CC.newWebSocketProvider ev.gardenEthereumNodeWebSocket
  web3 <- lift $ CC.newWeb3 provider
  pure web3

getCirclesCore :: forall r. Web3 -> EnvVars -> ExceptV ( errNative :: Error | r ) Effect CirclesCore
getCirclesCore web3 ev =
  CC.newCirclesCore web3
    { apiServiceEndpoint: ev.gardenApi
    , graphNodeEndpoint: ev.gardenGraphApi
    , hubAddress: ev.gardenHubAddress
    , proxyFactoryAddress: ev.gardenProxyFactoryAddress
    , relayServiceEndpoint: ev.gardenRelay
    , safeMasterAddress: ev.gardenSafeMasterAddress
    , subgraphName: ev.gardenSubgraphName
    , databaseSource: "graph"
    }

testEnv :: C.Env Identity
testEnv =
  { apiCheckUserName: \_ -> pure { isValid: true }
  , apiCheckEmail: \_ -> pure { isValid: true }
  , generatePrivateKey: pure zeroKey
  , userRegister: \_ _ -> pure unit
  , getSafeAddress: \_ -> pure sampleAddress
  , safePrepareDeploy: \_ -> pure sampleAddress
  , userResolve:
      \_ ->
        pure
          { id: 0
          , username: ""
          , safeAddress: sampleAddress
          , avatarUrl: ""
          }
  }
