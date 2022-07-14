module CirclesPink.Garden.StateMachine.Control.Env
  ( AddTrustConnection
  , ApiCheckEmail
  , ApiCheckUserName
  , CheckUBIPayout
  , CoreToWindow
  , DeploySafe
  , DeployToken
  , Env
  , ErrAddTrustConnection
  , ErrCheckUBIPayout
  , ErrCoreToWindow
  , ErrDecode
  , ErrDeploySafe
  , ErrDeployToken
  , ErrGetBalance
  , ErrGetSafeAddress
  , ErrGetSafeStatus
  , ErrGetUsers
  , ErrInvalidMnemonic
  , ErrIsFunded
  , ErrIsTrusted
  , ErrPrepareSafeDeploy
  , ErrPrivKeyToSafeAddress
  , ErrReadStorage
  , ErrRemoveTrustConnection
  , ErrRequestUBIPayout
  , ErrRestoreSession
  , ErrSaveSession
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
  , Sleep
  , Transfer
  , TrustGetNetwork
  , UserNotFoundError
  , UserRegister
  , UserResolve
  , UserSearch
  , _errDecode
  , _errReadStorage
  ) where

import Prelude

import CirclesCore (ErrApi, ErrInvalidUrl, ErrNative, ErrService, SafeStatus, TrustIsTrustedResult, TrustNode, User, UserOptions, ErrParseAddress)
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (JsonDecodeError)
import Data.BN (BN)
import Data.DateTime.Instant (Instant)
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

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
-- | Trust Add Connection
type ErrAddTrustConnection r = ErrNative + ErrInvalidUrl + r

type AddTrustConnection m = forall r. PrivateKey -> Address -> Address -> ExceptV (ErrAddTrustConnection + r) m String

-- | Trust Remove Connection
type ErrRemoveTrustConnection r = ErrNative + ErrInvalidUrl + r

type RemoveTrustConnection m = forall r. PrivateKey -> Address -> Address -> ExceptV (ErrRemoveTrustConnection + r) m String

-- | Save Session
type ErrSaveSession r = (errSaveSession :: Unit | r)

type SaveSession m = forall r. PrivateKey -> ExceptV (ErrSaveSession + r) m Unit

-- | Restore Session
type RequestPath = Array String

type ErrReadStorage r = (errReadStorage :: RequestPath | r)

type ErrDecode r = (errDecode :: JsonDecodeError | r)

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
type Env m =
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
  , saveSession :: SaveSession m
  , restoreSession :: RestoreSession m
  , getBalance :: GetBalance m
  , checkUBIPayout :: CheckUBIPayout m
  , requestUBIPayout :: RequestUBIPayout m
  , transfer :: Transfer m
  , getTimestamp :: GetTimestamp m
  , sleep :: Sleep m
  , logInfo :: LogInfo m
  }

--------------------------------------------------------------------------------
-- Err constructors
--------------------------------------------------------------------------------
_errReadStorage :: forall r. RequestPath -> Variant (ErrReadStorage r)
_errReadStorage = inj (Proxy :: _ "errReadStorage")

_errDecode :: forall r. JsonDecodeError -> Variant (ErrDecode r)
_errDecode = inj (Proxy :: _ "errDecode")
