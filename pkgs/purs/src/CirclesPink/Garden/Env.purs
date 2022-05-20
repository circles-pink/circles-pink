module CirclesPink.Garden.Env
  ( EnvVars(..)
  , env
  , liftEnv
  , testEnv
  ) where

import Prelude

import CirclesCore (CirclesCore, ErrInvalidUrl, ErrNative, Web3)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env (_errDecode, _errReadStorage)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (class MonadTrans, ExceptT(..), except, lift, mapExceptT, runExceptT, throwError)
import Control.Monad.Except.Checked (ExceptV)
import Convertable (convert)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Newtype.Extra ((-|))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Timer (clearTimeout, setTimeout)
import GunDB (get, offline, once, put)
import HTTP (ReqFn)
import Record.Extra.CirclesPink (zipRecord)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Undefined (undefined)
import Wallet.PrivateKey (sampleAddress, sampleKey)
import Wallet.PrivateKey as P

--------------------------------------------------------------------------------
newtype EnvVars = EnvVars
  { gardenApi :: String
  , gardenApiUsers :: String
  , gardenGraphApi :: String
  , gardenSubgraphName :: String
  , gardenRelay :: String
  , gardenHubAddress :: String
  , gardenProxyFactoryAddress :: String
  , gardenSafeMasterAddress :: String
  , gardenEthereumNodeWebSocket :: String
  }

derive instance newtypeEnvVars :: Newtype EnvVars _

--------------------------------------------------------------------------------
_errService :: CirclesError
_errService = inj (Proxy :: _ "errService") unit

_errParse :: CirclesError
_errParse = inj (Proxy :: _ "errParse") unit

env :: { request :: ReqFn (CirclesError' ()), envVars :: EnvVars } -> Env.Env Aff
env { request, envVars } =
  { apiCheckUserName
  , apiCheckEmail
  , generatePrivateKey
  , userRegister
  , getUsers
  , userSearch
  , getSafeAddress
  , safePrepareDeploy
  , userResolve
  , coreToWindow
  , isTrusted
  , trustGetNetwork
  , getSafeStatus
  , deploySafe
  , deployToken
  , isFunded
  , addTrustConnection
  , removeTrustConnection
  , saveSession
  , restoreSession
  , getBalance
  , checkUBIPayout
  , requestUBIPayout
  , transfer
  , getTimestamp
  , sleep
  }
  where
  apiCheckUserName :: Env.ApiCheckUserName Aff
  apiCheckUserName username =
    if username == "" then
      pure { isValid: false }
    else
      request
        { url: envVars -| _.gardenApiUsers
        , method: POST
        , body: encodeJson { username }
        }
        # runExceptT
        <#>
          ( \result -> case result of
              Left e -> Left e
              Right x
                | x.status /= 200 && x.status /= 409 -> Left $ _errService
                | otherwise -> Right x
          )
        <#>
          ( \result -> do
              res <- result
              body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
              if body'.status == "ok" then
                Right { isValid: true }
              else
                Right { isValid: false }
          )
        # ExceptT

  apiCheckEmail :: Env.ApiCheckEmail Aff
  apiCheckEmail email =
    if email == "" then
      pure { isValid: false }
    else
      request
        { url: envVars -| _.gardenApiUsers
        , method: POST
        , body: encodeJson { email }
        }
        # runExceptT
        <#>
          ( \result -> case result of
              Left e -> Left e
              Right x
                | x.status /= 200 && x.status /= 409 -> Left $ _errService
                | otherwise -> Right x
          )
        <#>
          ( \result -> do
              res <- result
              body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
              if body'.status == "ok" then
                Right { isValid: true }
              else
                Right { isValid: false }
          )
        # ExceptT

  generatePrivateKey :: Env.GeneratePrivateKey Aff
  generatePrivateKey = P.genPrivateKey

  userRegister :: Env.UserRegister Aff
  userRegister privKey options = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    -- safeAddress <- getSafeAddress privKey
    --------------------------------------------------------------------------------
    -- This Section cannot go to production! it auto-funds safe of user on register
    --------------------------------------------------------------------------------
    -- _ <-
    --   mapExceptT liftEffect
    --     $ CC.sendTransaction
    --         web3
    --         (P.unsafeAddrFromString "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1")
    --         safeAddress
    --------------------------------------------------------------------------------
    -- Section end .. (prod addr 0x450Fc3eAc8b847a759Cb05790cE7A1f465ac0cE8)
    --------------------------------------------------------------------------------
    CC.userRegister circlesCore account options

  userSearch :: Env.UserSearch Aff
  userSearch privKey options = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.userSearch circlesCore account options

  getSafeAddress :: Env.GetSafeAddress Aff
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

  safePrepareDeploy :: Env.PrepareSafeDeploy Aff
  safePrepareDeploy privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    CC.safePrepareDeploy circlesCore account { nonce: nonce }

  userResolve :: Env.UserResolve Aff
  userResolve privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    safeAddress <- getSafeAddress privKey
    users <- CC.userResolve circlesCore account { userNames: [], addresses: [ safeAddress ] }
    case head users of
      Nothing -> throwError (inj (Proxy :: _ "errUserNotFound") { safeAddress })
      Just u -> pure u

  getUsers :: Env.GetUsers Aff
  getUsers privKey userNames addresses = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    CC.userResolve circlesCore account { userNames, addresses }

  coreToWindow :: Env.CoreToWindow Aff
  coreToWindow privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.unsafeSampleCore circlesCore account
    log "Debug: sampleCore and sampleAccount written to global window object"
    pure unit

  isTrusted :: Env.IsTrusted Aff
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

  isFunded :: Env.IsFunded Aff
  isFunded privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = P.privKeyToAddress privKey
    let
      nonce = P.addressToNonce address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.safeIsFunded circlesCore account { safeAddress }

  trustGetNetwork :: Env.TrustGetNetwork Aff
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

  getSafeStatus :: Env.GetSafeStatus Aff
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

  deploySafe :: Env.DeploySafe Aff
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

  deployToken :: Env.DeployToken Aff
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

  addTrustConnection :: Env.AddTrustConnection Aff
  addTrustConnection pk other us = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 pk
    CC.trustAddConnection circlesCore account { user: convert other, canSendTo: convert us }

  removeTrustConnection :: Env.RemoveTrustConnection Aff
  removeTrustConnection pk other us = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 pk
    CC.trustRemoveConnection circlesCore account { user: convert other, canSendTo: convert us }

  saveSession :: Env.SaveSession Aff
  saveSession privKey = do
    gundb <- liftEffect $ offline
    _ <- liftEffect $ gundb # get privateKeyStore # put (encodeJson { privKey })
    pure unit

  restoreSession :: Env.RestoreSession Aff
  restoreSession = do
    gundb <- lift $ liftEffect $ offline
    result <- gundb # get privateKeyStore # once <#> note (_errReadStorage [ privateKeyStore ]) # ExceptT
    resultPk :: { privKey :: _ } <- decodeJson result.data # lmap _errDecode # except
    pure resultPk.privKey

  getBalance :: Env.GetBalance Aff
  getBalance privKey safeAddress = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenGetBalance circlesCore account { safeAddress }

  checkUBIPayout :: Env.CheckUBIPayout Aff
  checkUBIPayout privKey safeAddress = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenCheckUBIPayout circlesCore account { safeAddress }

  requestUBIPayout :: Env.RequestUBIPayout Aff
  requestUBIPayout privKey safeAddress = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenRequestUBIPayout circlesCore account { safeAddress }

  transfer :: Env.Transfer Aff
  transfer privKey from to value paymentNote = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    bn <- lift $ liftEffect $ CC.strToBN value
    CC.tokenTransfer circlesCore account { from, to, value: bn, paymentNote }

  getTimestamp :: Env.GetTimestamp Aff
  getTimestamp = liftEffect now

  sleep :: Env.Sleep Aff
  sleep t =
    makeAff \f -> do
      id <- setTimeout t $ f $ Right unit
      pure $ Canceler $ \_ -> liftEffect $ clearTimeout id

privateKeyStore :: String
privateKeyStore = "session"

getWeb3 :: forall r. EnvVars -> ExceptV (ErrNative + ErrInvalidUrl + r) Effect Web3
getWeb3 ev = do
  provider <- CC.newWebSocketProvider $ ev -| _.gardenEthereumNodeWebSocket
  web3 <- lift $ CC.newWeb3 provider
  pure web3

getCirclesCore :: forall r. Web3 -> EnvVars -> ExceptV (ErrNative + r) Effect CirclesCore
getCirclesCore web3 ev =
  CC.newCirclesCore web3
    { apiServiceEndpoint: ev -| _.gardenApi
    , graphNodeEndpoint: ev -| _.gardenGraphApi
    , hubAddress: ev -| _.gardenHubAddress
    , proxyFactoryAddress: ev -| _.gardenProxyFactoryAddress
    , relayServiceEndpoint: ev -| _.gardenRelay
    , safeMasterAddress: ev -| _.gardenSafeMasterAddress
    , subgraphName: ev -| _.gardenSubgraphName
    , databaseSource: "graph"
    }

testEnv :: Env.Env Identity
testEnv =
  { apiCheckUserName: \_ -> pure { isValid: true }
  , apiCheckEmail: \_ -> pure { isValid: true }
  , generatePrivateKey: pure sampleKey
  , userRegister: \_ _ -> pure unit
  , userSearch: \_ _ -> pure []
  , getSafeAddress: \_ -> pure sampleAddress
  , safePrepareDeploy: \_ -> pure sampleAddress
  , userResolve: \_ -> pure { id: 0, username: "", safeAddress: sampleAddress, avatarUrl: "" }
  , getUsers: \_ _ _ -> pure []
  , coreToWindow: \_ -> pure unit
  , isTrusted: \_ -> pure $ wrap { isTrusted: false, trustConnections: 0 }
  , trustGetNetwork: \_ -> pure []
  , getSafeStatus: \_ -> pure { isCreated: false, isDeployed: false }
  , deploySafe: \_ -> pure unit
  , deployToken: \_ -> pure ""
  , isFunded: \_ -> pure false
  , addTrustConnection: \_ _ _ -> pure ""
  , removeTrustConnection: \_ _ _ -> pure ""
  , saveSession: \_ -> pure unit
  , restoreSession: pure P.sampleKey
  , getBalance: \_ _ -> pure $ wrap { length: 0, negative: 0, red: false, words: [] }
  , checkUBIPayout: \_ _ -> pure $ wrap { length: 0, negative: 0, red: false, words: [] }
  , requestUBIPayout: \_ _ -> pure ""
  , transfer: \_ _ _ _ _ -> pure ""
  , getTimestamp: pure bottom
  , sleep: \_ -> pure unit
  }

--------------------------------------------------------------------------------

liftVal :: forall e m a t. MonadTrans t => Monad m => ExceptT e m a -> ExceptT e (t m) a
liftVal f = f # mapExceptT lift

liftFn1 :: forall a1 e m a t. MonadTrans t => Monad m => (a1 -> ExceptT e m a) -> a1 -> (ExceptT e (t m) a)
liftFn1 f x1 = f x1 # mapExceptT lift

liftFn2 :: forall a1 a2 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> ExceptT e m a) -> a1 -> a2 -> (ExceptT e (t m) a)
liftFn2 f x1 x2 = f x1 x2 # mapExceptT lift

liftFn3 :: forall a1 a2 a3 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> a3 -> ExceptT e m a) -> a1 -> a2 -> a3 -> (ExceptT e (t m) a)
liftFn3 f x1 x2 x3 = f x1 x2 x3 # mapExceptT lift

liftFn4 :: forall a1 a2 a3 a4 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> a3 -> a4 -> ExceptT e m a) -> a1 -> a2 -> a3 -> a4 -> (ExceptT e (t m) a)
liftFn4 f x1 x2 x3 x4 = f x1 x2 x3 x4 # mapExceptT lift

liftFn5 :: forall a1 a2 a3 a4 a5 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> ExceptT e m a) -> a1 -> a2 -> a3 -> a4 -> a5 -> (ExceptT e (t m) a)
liftFn5 f x1 x2 x3 x4 x5 = f x1 x2 x3 x4 x5 # mapExceptT lift

liftEnv :: forall t m. MonadTrans t => Monad m => Env.Env m -> Env.Env (t m)
liftEnv e =
  { apiCheckUserName: liftFn1 e.apiCheckUserName
  , apiCheckEmail: liftFn1 e.apiCheckEmail
  , generatePrivateKey: lift e.generatePrivateKey
  , userRegister: liftFn2 e.userRegister
  , userSearch: liftFn2 e.userSearch
  , getSafeAddress: liftFn1 e.getSafeAddress
  , safePrepareDeploy: liftFn1 e.safePrepareDeploy
  , userResolve: liftFn1 e.userResolve
  , getUsers: liftFn3 e.getUsers
  , coreToWindow: liftFn1 e.coreToWindow
  , isTrusted: liftFn1 e.isTrusted
  , trustGetNetwork: liftFn1 e.trustGetNetwork
  , getSafeStatus: liftFn1 e.getSafeStatus
  , deploySafe: liftFn1 e.deploySafe
  , deployToken: liftFn1 e.deployToken
  , isFunded: liftFn1 e.isFunded
  , addTrustConnection: liftFn3 e.addTrustConnection
  , removeTrustConnection: liftFn3 e.removeTrustConnection
  , saveSession: liftFn1 e.saveSession
  , restoreSession: liftVal e.restoreSession
  , getBalance: liftFn2 e.getBalance
  , checkUBIPayout: liftFn2 e.checkUBIPayout
  , requestUBIPayout: liftFn2 e.requestUBIPayout
  , transfer: liftFn5 e.transfer
  , getTimestamp: lift e.getTimestamp
  , sleep: lift <<< e.sleep
  }

--------------------------------------------------------------------------------