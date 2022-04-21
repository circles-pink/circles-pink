module CirclesPink.Garden.Env
  ( env, testEnv, EnvVars
  ) where

import Prelude
import CirclesCore (CirclesCore, ErrNative, Web3, ErrInvalidUrl, userResolve)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env as E
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
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import HTTP (ReqFn)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
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

env :: { request :: ReqFn (CirclesError' ()), envVars :: EnvVars } -> E.Env Aff
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
  , getSafeAddress
  , safePrepareDeploy:
      \privKey -> do
        web3 <- mapExceptT liftEffect $ getWeb3 envVars
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
        let
          address = P.privKeyToAddress privKey
        let
          nonce = P.addressToNonce address
        CC.safePrepareDeploy circlesCore account { nonce: nonce }
  , userResolve:
      \privKey -> do
        web3 <- mapExceptT liftEffect $ getWeb3 envVars
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
        safeAddress <- getSafeAddress privKey
        users <- userResolve circlesCore account { userNames: [], addresses: [ safeAddress ] }
        case head users of
          Nothing -> throwError (inj (Proxy :: _ "errUserNotFound") { safeAddress })
          Just u -> pure u
  , coreToWindow
  , isTrusted
  , trustGetNetwork
  , getSafeStatus
  , deploySafe
  , deployToken
  }
  where
  apiCheckUserName :: E.EnvApiCheckUserName Aff
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

  apiCheckEmail :: E.EnvApiCheckEmail Aff
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

  getSafeAddress :: E.EnvGetSafeAddress Aff
  getSafeAddress privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    pure safeAddress

  coreToWindow :: E.EnvCoreToWindow Aff
  coreToWindow privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.unsafeSampleCore circlesCore account
    log "Debug: sampleCore and sampleAccount written to global window object"
    pure unit

  isTrusted :: E.EnvIsTrusted Aff
  isTrusted privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.trustIsTrusted circlesCore account { safeAddress, limit: 3 }

  trustGetNetwork :: E.EnvTrustGetNetwork Aff
  trustGetNetwork privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.trustGetNetwork circlesCore account { safeAddress }

  getSafeStatus :: E.EnvGetSafeStatus Aff
  getSafeStatus privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.safeGetSafeStatus circlesCore account { safeAddress }

  deploySafe :: E.EnvDeploySafe Aff
  deploySafe privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.safeDeploy circlesCore account { safeAddress }

  deployToken :: E.EnvDeployToken Aff
  deployToken privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.tokenDeploy circlesCore account { safeAddress }

getWeb3 :: forall r. EnvVars -> ExceptV (ErrNative + ErrInvalidUrl + r) Effect Web3
getWeb3 ev = do
  provider <- CC.newWebSocketProvider ev.gardenEthereumNodeWebSocket
  web3 <- lift $ CC.newWeb3 provider
  pure web3

getCirclesCore :: forall r. Web3 -> EnvVars -> ExceptV (ErrNative + r) Effect CirclesCore
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

testEnv :: E.Env Identity
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
  , coreToWindow: \_ -> pure unit
  , isTrusted: \_ -> pure { isTrusted: false, trustConnections: 0 }
  , trustGetNetwork: \_ -> pure []
  , getSafeStatus:
      \_ ->
        pure
          { isCreated: false
          , isDeployed: false
          }
  , deploySafe:
      \_ ->
        pure unit
  , deployToken:
      \_ ->
        pure ""
  }
