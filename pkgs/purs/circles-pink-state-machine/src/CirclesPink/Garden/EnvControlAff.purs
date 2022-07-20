module CirclesPink.Garden.EnvControlAff
  ( EnvVars(..)
  , env
  ) where

import Prelude

import CirclesCore (CirclesCore, ErrInvalidUrl, ErrNative, Web3)
import CirclesCore as CC
import CirclesPink.Data.Nonce (addressToNonce)
import CirclesPink.Data.PrivateKey (genPrivateKey)
import CirclesPink.Garden.StateMachine.Control.EnvControl (CryptoKey(..), EnvControl, ErrParseToData, ErrParseToJson, StorageType(..), ErrDecrypt, _errDecode, _errKeyNotFound, _errNoStorage, _errParseToData, _errParseToJson, _errReadStorage)
import CirclesPink.Garden.StateMachine.Control.EnvControl as EnvControl
import CirclesPink.Garden.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (ExceptT(..), except, lift, mapExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Convertable (convert)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, parseJson, stringify)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Newtype.Extra ((-#))
import Data.Tuple.Nested ((/\))
import Data.Variant (inj)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Timer (clearTimeout, setTimeout)
import GunDB (get, offline, once, put)
import HTTP (ReqFn)
import Network.Ethereum.Core.Signatures (privateToAddress)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

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

type EnvEnvControlAff =
  { request :: ReqFn (CirclesError' ())
  , localStorage ::
      Maybe
        { setItem :: String -> String -> Aff Unit
        , getItem :: String -> ExceptT Unit Aff String
        , deleteItem :: String -> ExceptT Unit Aff Unit
        , clear :: Aff Unit
        }
  , sessionStorage ::
      Maybe
        { setItem :: String -> String -> Aff Unit
        , getItem :: String -> ExceptT Unit Aff String
        , deleteItem :: String -> ExceptT Unit Aff Unit
        , clear :: Aff Unit
        }
  , crypto ::
      { encrypt :: CryptoKey -> String -> String
      , decrypt :: CryptoKey -> String -> Maybe String
      }
  , envVars :: EnvVars
  }

env
  :: EnvEnvControlAff
  -> EnvControl Aff
env { localStorage, sessionStorage, crypto, request, envVars } =
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
  , privKeyToSafeAddress
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
  , logInfo
  , storageSetItem
  , storageGetItem
  , storageDeleteItem
  , storageClear
  }
  where
  apiCheckUserName :: EnvControl.ApiCheckUserName Aff
  apiCheckUserName username =
    if username == "" then
      pure { isValid: false }
    else
      request
        { url: envVars -# _.gardenApiUsers
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

  apiCheckEmail :: EnvControl.ApiCheckEmail Aff
  apiCheckEmail email =
    if email == "" then
      pure { isValid: false }
    else
      request
        { url: envVars -# _.gardenApiUsers
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

  generatePrivateKey :: EnvControl.GeneratePrivateKey Aff
  generatePrivateKey = genPrivateKey

  userRegister :: EnvControl.UserRegister Aff
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

  userSearch :: EnvControl.UserSearch Aff
  userSearch privKey options = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.userSearch circlesCore account options

  getSafeAddress :: EnvControl.GetSafeAddress Aff
  getSafeAddress privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    pure safeAddress

  safePrepareDeploy :: EnvControl.PrepareSafeDeploy Aff
  safePrepareDeploy privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    CC.safePrepareDeploy circlesCore account { nonce: nonce }

  userResolve :: EnvControl.UserResolve Aff
  userResolve privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    safeAddress <- getSafeAddress privKey
    users <- CC.userResolve circlesCore account { userNames: [], addresses: [ convert safeAddress ] }
    case head users of
      Nothing -> throwError (inj (Proxy :: _ "errUserNotFound") { safeAddress })
      Just u -> pure u

  getUsers :: EnvControl.GetUsers Aff
  getUsers privKey userNames addresses = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    CC.userResolve circlesCore account { userNames, addresses: map convert addresses }

  coreToWindow :: EnvControl.CoreToWindow Aff
  coreToWindow privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.unsafeSampleCore circlesCore account
    log "Debug: sampleCore, sampleAccount and BN library written to global window object"
    pure unit

  isTrusted :: EnvControl.IsTrusted Aff
  isTrusted privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.trustIsTrusted circlesCore account { safeAddress: convert safeAddress, limit: 3 }

  isFunded :: EnvControl.IsFunded Aff
  isFunded privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.safeIsFunded circlesCore account { safeAddress: convert safeAddress }

  trustGetNetwork :: EnvControl.TrustGetNetwork Aff
  trustGetNetwork privKey address = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.trustGetNetwork circlesCore account { safeAddress: convert address }

  privKeyToSafeAddress :: EnvControl.PrivKeyToSafeAddress Aff
  privKeyToSafeAddress privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    pure safeAddress

  getSafeStatus :: EnvControl.GetSafeStatus Aff
  getSafeStatus privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.safeGetSafeStatus circlesCore account { safeAddress: convert safeAddress }

  deploySafe :: EnvControl.DeploySafe Aff
  deploySafe privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.safeDeploy circlesCore account { safeAddress: convert safeAddress }

  deployToken :: EnvControl.DeployToken Aff
  deployToken privKey = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    let
      address = privateToAddress $ unwrap privKey
    let
      nonce = addressToNonce $ wrap address
    safeAddress <- CC.safePredictAddress circlesCore account { nonce: nonce }
    CC.tokenDeploy circlesCore account { safeAddress: convert safeAddress }

  addTrustConnection :: EnvControl.AddTrustConnection Aff
  addTrustConnection pk other us = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 pk
    CC.trustAddConnection circlesCore account { user: convert other, canSendTo: convert us }

  removeTrustConnection :: EnvControl.RemoveTrustConnection Aff
  removeTrustConnection pk other us = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 pk
    CC.trustRemoveConnection circlesCore account { user: convert other, canSendTo: convert us }

  saveSession :: EnvControl.SaveSession Aff
  saveSession privKey = do
    gundb <- liftEffect $ offline
    _ <- liftEffect $ gundb # get privateKeyStore # put (encodeJson { privKey })
    pure unit

  restoreSession :: EnvControl.RestoreSession Aff
  restoreSession = do
    gundb <- lift $ liftEffect $ offline
    result <- gundb # get privateKeyStore # once <#> note (_errReadStorage [ privateKeyStore ]) # ExceptT
    resultPk :: { privKey :: _ } <- decodeJson result.data # lmap _errDecode # except
    pure resultPk.privKey

  getBalance :: EnvControl.GetBalance Aff
  getBalance privKey safeAddress = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenGetBalance circlesCore account { safeAddress: convert safeAddress }

  checkUBIPayout :: EnvControl.CheckUBIPayout Aff
  checkUBIPayout privKey safeAddress = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenCheckUBIPayout circlesCore account { safeAddress: convert safeAddress }

  requestUBIPayout :: EnvControl.RequestUBIPayout Aff
  requestUBIPayout privKey safeAddress = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenRequestUBIPayout circlesCore account { safeAddress: convert safeAddress }

  transfer :: EnvControl.Transfer Aff
  transfer privKey from to value paymentNote = do
    web3 <- mapExceptT liftEffect $ getWeb3 envVars
    circlesCore <- mapExceptT liftEffect $ getCirclesCore web3 envVars
    account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
    CC.tokenTransfer circlesCore account { from: convert from, to: convert to, value, paymentNote }

  getTimestamp :: EnvControl.GetTimestamp Aff
  getTimestamp = liftEffect now

  sleep :: EnvControl.Sleep Aff
  sleep t =
    makeAff \f -> do
      id <- setTimeout t $ f $ Right unit
      pure $ Canceler $ \_ -> liftEffect $ clearTimeout id

  logInfo :: EnvControl.LogInfo Aff
  logInfo = log

  storageSetItem :: EnvControl.StorageSetItem Aff
  storageSetItem sk st k v = case st of
    LocalStorage -> case localStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ls -> ls.setItem (encryptJSON env sk k) (encryptJSON env sk v)
        # liftAff
    SessionStorage -> case sessionStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ss -> ss.setItem (stringify $ encodeJson k) (stringify $ encodeJson v)
        # liftAff

  storageGetItem :: EnvControl.StorageGetItem Aff
  storageGetItem _ st k = case st of
    LocalStorage -> case localStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ls -> ls.getItem (stringify $ encodeJson k)
        # withExceptT (const $ _errKeyNotFound k)
        >>= (\s -> parseJson s # except # withExceptT (\jde -> _errParseToJson s))
        >>= (\j -> decodeJson j # except # withExceptT (\jde -> _errParseToData (j /\ jde)))
    SessionStorage -> case sessionStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ss -> ss.getItem (stringify $ encodeJson k)
        # withExceptT (const $ _errKeyNotFound k)
        >>= (\s -> parseJson s # except # withExceptT (\jde -> _errParseToJson s))
        >>= (\j -> decodeJson j # except # withExceptT (\jde -> _errParseToData (j /\ jde)))

  storageDeleteItem :: EnvControl.StorageDeleteItem Aff
  storageDeleteItem _ st k = case st of
    LocalStorage -> case localStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ls -> ls.deleteItem (stringify $ encodeJson k) # withExceptT (const $ _errKeyNotFound k)
    SessionStorage -> case sessionStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ss -> ss.deleteItem (stringify $ encodeJson k) # withExceptT (const $ _errKeyNotFound k)

  storageClear :: EnvControl.StorageClear Aff
  storageClear st = case st of
    LocalStorage -> case localStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ls -> ExceptT $ Right <$> ls.clear
    SessionStorage -> case sessionStorage of
      Nothing -> throwError $ _errNoStorage st
      Just ss -> ExceptT $ Right <$> ss.clear

encryptJSON :: forall a. EncodeJson a => EnvEnvControlAff -> CryptoKey -> a -> String
encryptJSON { crypto: { encrypt } } sk k = encrypt sk $ stringify $ encodeJson k

decryptJSON :: forall a r. DecodeJson a => EnvEnvControlAff -> CryptoKey -> String -> Either (ErrDecryptJson r) a
decryptJSON { crypto: { decrypt } } sk v = todo

-- decrypt sk v
-- <#> decodeJson v 
-- <#> 

-- decrypt sk $ stringify $ decodeJson v

type ErrDecryptJson r = ErrParseToData + ErrParseToJson + ErrDecrypt + r

privateKeyStore :: String
privateKeyStore = "session"

getWeb3 :: forall r. EnvVars -> ExceptV (ErrNative + ErrInvalidUrl + r) Effect Web3
getWeb3 ev = do
  provider <- CC.newWebSocketProvider $ ev -# _.gardenEthereumNodeWebSocket
  web3 <- lift $ CC.newWeb3 provider
  pure web3

getCirclesCore :: forall r. Web3 -> EnvVars -> ExceptV (ErrNative + r) Effect CirclesCore
getCirclesCore web3 ev =
  CC.newCirclesCore web3
    { apiServiceEndpoint: ev -# _.gardenApi
    , graphNodeEndpoint: ev -# _.gardenGraphApi
    , hubAddress: ev -# _.gardenHubAddress
    , proxyFactoryAddress: ev -# _.gardenProxyFactoryAddress
    , relayServiceEndpoint: ev -# _.gardenRelay
    , safeMasterAddress: ev -# _.gardenSafeMasterAddress
    , subgraphName: ev -# _.gardenSubgraphName
    , databaseSource: "graph"
    }