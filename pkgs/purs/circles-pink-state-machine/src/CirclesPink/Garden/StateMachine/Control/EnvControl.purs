module CirclesPink.Garden.StateMachine.Control.EnvControl
  ( AddTrustConnection
  , ApiCheckEmail
  , ApiCheckUserName
  , CheckUBIPayout
  , CoreToWindow
  , CryptoKey(..)
  , DeploySafe
  , DeployToken
  , EnvControl
  , ErrAddTrustConnection
  , ErrCheckUBIPayout
  , ErrCoreToWindow
  , ErrDecode
  , ErrDecrypt
  , ErrDeploySafe
  , ErrDeployToken
  , ErrGetBalance
  , ErrGetSafeAddress
  , ErrGetSafeStatus
  , ErrGetUsers
  , ErrGetVoucherProviders
  , ErrGetVouchers
  , ErrInvalidMnemonic
  , ErrIsFunded
  , ErrIsTrusted
  , ErrKeyNotFound
  , ErrNoStorage
  , ErrParseToData
  , ErrParseToJson
  , ErrPrepareSafeDeploy
  , ErrPrivKeyToSafeAddress
  , ErrReadStorage
  , ErrRemoveTrustConnection
  , ErrRequestUBIPayout
  , ErrRestoreSession
  , ErrSaveSession
  , ErrStorageClear
  , ErrStorageDeleteItem
  , ErrStorageGetItem
  , ErrStorageSetItem
  , ErrTransfer
  , ErrTrustGetNetwork
  , ErrUserRegister
  , ErrUserResolve
  , ErrUserSearch
  , GeneratePrivateKey
  , GetBalance
  , GetSafeAddress
  , GetSafeStatus
  , GetTimestamp
  , GetUsers
  , GetVoucherProviders
  , GetVouchers
  , IsFunded
  , IsTrusted
  , LogInfo
  , PrepareSafeDeploy
  , PrivKeyToSafeAddress
  , RemoveTrustConnection
  , RequestPath
  , RequestUBIPayout
  , RestoreSession
  , SaveSession
  , SignChallenge
  , Sleep
  , StorageClear
  , StorageDeleteItem
  , StorageGetItem
  , StorageSetItem
  , StorageType(..)
  , Transfer
  , TrustGetNetwork
  , UserNotFoundError
  , UserRegister
  , UserResolve
  , UserSearch
  , _errDecode
  , _errDecrypt
  , _errGetVoucherProviders
  , _errGetVouchers
  , _errKeyNotFound
  , _errNoStorage
  , _errParseToData
  , _errParseToJson
  , _errReadStorage
  ) where

import Prelude

import CirclesCore (ErrApi, ErrInvalidUrl, ErrNative, ErrService, SafeStatus, TrustIsTrustedResult, TrustNode, UserOptions, ErrParseAddress)
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Data.User (User)
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError)
import Data.BN (BN)
import Data.DateTime.Instant (Instant)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import VoucherServer.Types (Voucher, VoucherProvider)
import Web3 (Message, SignatureObj)

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------
type UserNotFoundError =
  { safeAddress :: Address
  }

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------
type ApiCheckUserName m = String -> ExceptT CirclesError m { isValid :: Boolean }

--------------------------------------------------------------------------------
type ApiCheckEmail m = String -> ExceptT CirclesError m { isValid :: Boolean }

--------------------------------------------------------------------------------
type ErrGetSafeAddress r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type GetSafeAddress m = forall r. PrivateKey -> ExceptV (ErrGetSafeAddress + r) m Address

--------------------------------------------------------------------------------
type ErrCoreToWindow r = ErrNative + ErrInvalidUrl + r

type CoreToWindow m = forall r. PrivateKey -> ExceptV (ErrCoreToWindow + r) m Unit

--------------------------------------------------------------------------------
type ErrIsTrusted r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type IsTrusted m = forall r. PrivateKey -> ExceptV (ErrIsTrusted + r) m TrustIsTrustedResult

--------------------------------------------------------------------------------
type ErrIsFunded r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type IsFunded m = forall r. PrivateKey -> ExceptV (ErrIsFunded + r) m Boolean
--------------------------------------------------------------------------------
type ErrInvalidMnemonic r = (errInvalidMnemonic :: Unit | r)

--------------------------------------------------------------------------------
type ErrTrustGetNetwork r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type TrustGetNetwork m = forall r. PrivateKey -> Address -> ExceptV (ErrTrustGetNetwork + r) m (Array TrustNode)

--------------------------------------------------------------------------------
type ErrPrivKeyToSafeAddress r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type PrivKeyToSafeAddress m = forall r. PrivateKey -> ExceptV (ErrPrivKeyToSafeAddress + r) m Address

--------------------------------------------------------------------------------
type ErrGetSafeStatus r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type GetSafeStatus m = forall r. PrivateKey -> ExceptV (ErrGetSafeStatus + r) m SafeStatus

--------------------------------------------------------------------------------
type ErrDeploySafe r = ErrService + ErrInvalidUrl + ErrNative + ErrParseAddress + r

type DeploySafe m = forall r. PrivateKey -> ExceptV (ErrDeploySafe + r) m Unit

--------------------------------------------------------------------------------
type ErrDeployToken r = ErrService + ErrInvalidUrl + ErrNative + ErrParseAddress + r

type DeployToken m = forall r. PrivateKey -> ExceptV (ErrDeployToken + r) m String

--------------------------------------------------------------------------------
type ErrUserResolve r = ErrNative + ErrInvalidUrl + ErrApi + ErrParseAddress + ErrParseAddress + (errUserNotFound :: UserNotFoundError | r)

type UserResolve m = forall r. PrivateKey -> ExceptV (ErrUserResolve + r) m User

--------------------------------------------------------------------------------
type ErrUserSearch r = ErrInvalidUrl + ErrNative + ErrApi + ErrParseAddress + r

type UserSearch m = forall r. PrivateKey -> { query :: String } -> ExceptV (ErrUserSearch + r) m (Array User)

--------------------------------------------------------------------------------
type ErrGetUsers r = ErrNative + ErrInvalidUrl + ErrApi + ErrParseAddress + (errUserNotFound :: UserNotFoundError | r)

type GetUsers m = forall r. PrivateKey -> (Array String) -> (Array Address) -> ExceptV (ErrGetUsers + r) m (Array User)

--------------------------------------------------------------------------------
type ErrPrepareSafeDeploy r = ErrNative + ErrInvalidUrl + ErrParseAddress + r

type PrepareSafeDeploy m = forall r. PrivateKey -> ExceptV (ErrPrepareSafeDeploy + r) m Address

--------------------------------------------------------------------------------
type ErrUserRegister r = ErrService + ErrNative + ErrInvalidUrl + r

type UserRegister m = forall r. PrivateKey -> UserOptions -> ExceptV (ErrUserRegister + r) m Unit

type GeneratePrivateKey :: forall k. (Type -> k) -> k
type GeneratePrivateKey m = m PrivateKey

--------------------------------------------------------------------------------
-- Trust Add Connection
type ErrAddTrustConnection r = ErrNative + ErrInvalidUrl + r

type AddTrustConnection m = forall r. PrivateKey -> Address -> Address -> ExceptV (ErrAddTrustConnection + r) m String

-- Trust Remove Connection
type ErrRemoveTrustConnection r = ErrNative + ErrInvalidUrl + r

type RemoveTrustConnection m = forall r. PrivateKey -> Address -> Address -> ExceptV (ErrRemoveTrustConnection + r) m String

--------------------------------------------------------------------------------

-- Sign Challenge
type SignChallenge m = Message -> PrivateKey -> m SignatureObj

--------------------------------------------------------------------------------

-- Get Vouchers

type ErrGetVouchers r = (errGetVouchers :: String | r)

type GetVouchers m = forall r. SignatureObj -> ExceptV (ErrGetVouchers + r) m (Array Voucher)

--------------------------------------------------------------------------------

-- Get VoucherProviders

type ErrGetVoucherProviders r = (errGetVoucherProviders :: String | r)

type GetVoucherProviders m = forall r. SignatureObj -> ExceptV (ErrGetVoucherProviders + r) m (Array VoucherProvider)

--------------------------------------------------------------------------------

-- Save Session
-- type ErrSaveSession r = ErrNoStorage + ErrStorageSetItem + r

-- type SaveSession m = forall r. PrivateKey -> ExceptV (ErrSaveSession + r) m Unit

-- Restore Session
type RequestPath = Array String

type ErrReadStorage r = (errReadStorage :: RequestPath | r)

type ErrDecode r = (errDecode :: JsonDecodeError | r)

-- type ErrRestoreSession k r = ErrNoStorage + ErrStorageGetItem k + r

-- type RestoreSession k m = forall r. ExceptV (ErrRestoreSession k + r) m PrivateKey

-- Save Session
type ErrSaveSession r = (errSaveSession :: Unit | r)

type SaveSession m = forall r. PrivateKey -> ExceptV (ErrSaveSession + r) m Unit

-- Restore Session

type ErrRestoreSession r = ErrReadStorage + ErrDecode + r

type RestoreSession m = forall r. ExceptV (ErrRestoreSession + r) m PrivateKey

--------------------------------------------------------------------------------
type ErrGetBalance r = ErrNative + ErrInvalidUrl + r

type GetBalance m = forall r. PrivateKey -> Address -> ExceptV (ErrGetBalance + r) m BN

--------------------------------------------------------------------------------
type ErrCheckUBIPayout r = ErrNative + ErrInvalidUrl + r

type CheckUBIPayout m = forall r. PrivateKey -> Address -> ExceptV (ErrCheckUBIPayout + r) m BN

--------------------------------------------------------------------------------
type ErrRequestUBIPayout r = ErrNative + ErrInvalidUrl + r

type RequestUBIPayout m = forall r. PrivateKey -> Address -> ExceptV (ErrRequestUBIPayout + r) m String

--------------------------------------------------------------------------------
type ErrTransfer r = ErrNative + ErrInvalidUrl + r

type Transfer m = forall r. PrivateKey -> Address -> Address -> BN -> String -> ExceptV (ErrTransfer + r) m String

--------------------------------------------------------------------------------
type GetTimestamp :: forall k. (Type -> k) -> k
type GetTimestamp m = m Instant

--------------------------------------------------------------------------------
type Sleep m = Int -> m Unit
--------------------------------------------------------------------------------
type LogInfo m = String -> m Unit

--------------------------------------------------------------------------------
-- Storage
--------------------------------------------------------------------------------

newtype CryptoKey = CryptoKey String

type StorageSetItem m = forall k v r. EncodeJson k => EncodeJson v => CryptoKey -> StorageType -> k -> v -> ExceptV (ErrStorageSetItem + r) m Unit

type StorageGetItem m = forall k v r. EncodeJson k => DecodeJson v => CryptoKey -> StorageType -> k -> ExceptV (ErrStorageGetItem k + r) m v

type StorageDeleteItem m = forall k r. EncodeJson k => CryptoKey -> StorageType -> k -> ExceptV (ErrStorageDeleteItem + r) m Unit

type StorageClear m = forall r. StorageType -> ExceptV (ErrStorageClear + r) m Unit

data StorageType = SessionStorage | LocalStorage

--------------------------------------------------------------------------------
type ErrStorageSetItem r = ErrNoStorage + r

type ErrStorageGetItem k r = ErrNoStorage + ErrParseToJson + ErrParseToData + ErrDecrypt + ErrKeyNotFound k + r

type ErrStorageDeleteItem r = ErrNoStorage + r

type ErrStorageClear r = ErrNoStorage + r

-- Error slices
type ErrParseToJson r = (errParseToJson :: String | r)

type ErrDecrypt r = (errDecrypt :: Unit | r)

type ErrParseToData r = (errParseToData :: String /\ JsonDecodeError | r)

type ErrNoStorage r = (errNoStorage :: StorageType | r)

type ErrKeyNotFound :: forall k1. k1 -> Row k1 -> Row k1
type ErrKeyNotFound k r = (errKeyNotFound :: k | r)

-- Error Connstructors

--------------------------------------------------------------------------------

_errParseToJson :: forall r. String -> Variant (ErrParseToJson r)
_errParseToJson = inj (Proxy :: _ "errParseToJson")

_errParseToData :: forall r. String /\ JsonDecodeError -> Variant (ErrParseToData r)
_errParseToData = inj (Proxy :: _ "errParseToData")

_errNoStorage :: forall r. StorageType -> Variant (ErrNoStorage r)
_errNoStorage = inj (Proxy :: _ "errNoStorage")

_errKeyNotFound :: forall r k. k -> Variant (ErrKeyNotFound k r)
_errKeyNotFound = inj (Proxy :: _ "errKeyNotFound")

_errDecrypt :: forall r. Variant (ErrDecrypt r)
_errDecrypt = inj (Proxy :: _ "errDecrypt") unit

_errGetVouchers :: forall r. String -> Variant (ErrGetVouchers r)
_errGetVouchers = inj (Proxy :: _ "errGetVouchers")

_errGetVoucherProviders :: forall r. String -> Variant (ErrGetVoucherProviders r)
_errGetVoucherProviders = inj (Proxy :: _ "errGetVoucherProviders")

--------------------------------------------------------------------------------

type EnvControl m =
  { apiCheckUserName :: ApiCheckUserName m
  , apiCheckEmail :: ApiCheckEmail m
  , generatePrivateKey :: GeneratePrivateKey m
  , userRegister :: UserRegister m
  , getSafeAddress :: GetSafeAddress m
  , safePrepareDeploy :: PrepareSafeDeploy m
  , userResolve :: UserResolve m
  , userSearch :: UserSearch m
  , getUsers :: GetUsers m
  , coreToWindow :: CoreToWindow m
  , isTrusted :: IsTrusted m
  , isFunded :: IsFunded m
  , trustGetNetwork :: TrustGetNetwork m
  , privKeyToSafeAddress :: PrivKeyToSafeAddress m
  , getSafeStatus :: GetSafeStatus m
  , deploySafe :: DeploySafe m
  , deployToken :: DeployToken m
  , addTrustConnection :: AddTrustConnection m
  , removeTrustConnection :: RemoveTrustConnection m
  , signChallenge :: SignChallenge m
  , getVouchers :: GetVouchers m
  , getVoucherProviders :: GetVoucherProviders m
  , saveSession :: SaveSession m
  , restoreSession :: RestoreSession m
  , getBalance :: GetBalance m
  , checkUBIPayout :: CheckUBIPayout m
  , requestUBIPayout :: RequestUBIPayout m
  , transfer :: Transfer m
  , getTimestamp :: GetTimestamp m
  , sleep :: Sleep m
  , logInfo :: LogInfo m
  , storageSetItem :: StorageSetItem m
  , storageGetItem :: StorageGetItem m
  , storageDeleteItem :: StorageDeleteItem m
  , storageClear :: StorageClear m
  }

--------------------------------------------------------------------------------
-- Err constructors
--------------------------------------------------------------------------------
_errReadStorage :: forall r. RequestPath -> Variant (ErrReadStorage r)
_errReadStorage = inj (Proxy :: _ "errReadStorage")

_errDecode :: forall r. JsonDecodeError -> Variant (ErrDecode r)
_errDecode = inj (Proxy :: _ "errDecode")
