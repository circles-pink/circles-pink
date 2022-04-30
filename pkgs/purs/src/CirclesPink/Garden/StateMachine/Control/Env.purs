module CirclesPink.Garden.StateMachine.Control.Env
  ( AddTrustConnection
  , ApiCheckEmail
  , ApiCheckUserName
  , CoreToWindow
  , DeploySafe
  , DeployToken
  , Env
  , ErrAddTrustConnection
  , ErrCoreToWindow
  , ErrDeploySafe
  , ErrDeployToken
  , ErrGetSafeAddress
  , ErrGetSafeStatus
  , ErrIsFunded
  , ErrIsTrusted
  , ErrPrepareSafeDeploy
  , ErrTrustGetNetwork
  , ErrUserRegister
  , ErrUserResolve
  , GeneratePrivateKey
  , GetSafeAddress
  , GetSafeStatus
  , IsFunded
  , IsTrusted
  , PrepareSafeDeploy
  , TrustGetNetwork
  , UserNotFoundError
  , UserRegister
  , UserResolve
  ) where

import Prelude
import CirclesCore (ErrApi, ErrInvalidUrl, ErrNative, ErrService, SafeStatus, TrustIsTrustedResult, TrustNode, User, UserOptions)
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey)

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------
type UserNotFoundError
  = { safeAddress :: Address
    }

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------
type ApiCheckUserName m
  = String -> ExceptT CirclesError m { isValid :: Boolean }

--------------------------------------------------------------------------------
type ApiCheckEmail m
  = String -> ExceptT CirclesError m { isValid :: Boolean }

--------------------------------------------------------------------------------
type ErrGetSafeAddress r
  = ErrNative + ErrInvalidUrl + r

type GetSafeAddress m
  = forall r. PrivateKey -> ExceptV (ErrGetSafeAddress + r) m Address

--------------------------------------------------------------------------------
type ErrCoreToWindow r
  = ErrNative + ErrInvalidUrl + r

type CoreToWindow m
  = forall r. PrivateKey -> ExceptV (ErrCoreToWindow + r) m Unit

--------------------------------------------------------------------------------
type ErrIsTrusted r
  = ErrNative + ErrInvalidUrl + r

type IsTrusted m
  = forall r. PrivateKey -> ExceptV (ErrIsTrusted + r) m TrustIsTrustedResult

--------------------------------------------------------------------------------
type ErrIsFunded r
  = ErrNative + ErrInvalidUrl + r

type IsFunded m
  = forall r. PrivateKey -> ExceptV (ErrIsFunded + r) m Boolean

--------------------------------------------------------------------------------
type ErrTrustGetNetwork r
  = ErrNative + ErrInvalidUrl + r

type TrustGetNetwork m
  = forall r. PrivateKey -> ExceptV (ErrTrustGetNetwork + r) m (Array TrustNode)

--------------------------------------------------------------------------------
type ErrGetSafeStatus r
  = ErrNative + ErrInvalidUrl + r

type GetSafeStatus m
  = forall r. PrivateKey -> ExceptV (ErrGetSafeStatus + r) m SafeStatus

--------------------------------------------------------------------------------
type ErrDeploySafe r
  = ErrService + ErrInvalidUrl + ErrNative + r

type DeploySafe m
  = forall r. PrivateKey -> ExceptV (ErrDeploySafe + r) m Unit

--------------------------------------------------------------------------------
type ErrDeployToken r
  = ErrService + ErrInvalidUrl + ErrNative + r

type DeployToken m
  = forall r. PrivateKey -> ExceptV (ErrDeployToken + r) m String

--------------------------------------------------------------------------------
type ErrUserResolve r
  = ErrNative + ErrInvalidUrl + ErrApi + ( errUserNotFound :: UserNotFoundError | r )

type UserResolve m
  = forall r. PrivateKey -> ExceptV (ErrUserResolve + r) m User

--------------------------------------------------------------------------------
type ErrPrepareSafeDeploy r
  = ErrNative + ErrInvalidUrl + r

type PrepareSafeDeploy m
  = forall r. PrivateKey -> ExceptV (ErrPrepareSafeDeploy + r) m Address

--------------------------------------------------------------------------------
type ErrUserRegister r
  = ErrService + ErrNative + ErrInvalidUrl + r

type UserRegister m
  = forall r. PrivateKey -> UserOptions -> ExceptV (ErrUserRegister + r) m Unit

--------------------------------------------------------------------------------
type GeneratePrivateKey m
  = forall r. ExceptV r m PrivateKey

--------------------------------------------------------------------------------
type ErrAddTrustConnection r
  = ErrNative + ErrInvalidUrl + r

type AddTrustConnection m
  = forall r. PrivateKey -> Address -> Address -> ExceptV (ErrAddTrustConnection + r) m String

--------------------------------------------------------------------------------
type Env m
  = { apiCheckUserName :: ApiCheckUserName m
    , apiCheckEmail :: ApiCheckEmail m
    , generatePrivateKey :: GeneratePrivateKey m
    , userRegister :: UserRegister m
    , getSafeAddress :: GetSafeAddress m
    , safePrepareDeploy :: PrepareSafeDeploy m
    , userResolve :: UserResolve m
    , coreToWindow :: CoreToWindow m
    , isTrusted :: IsTrusted m
    , isFunded :: IsFunded m
    , trustGetNetwork :: TrustGetNetwork m
    , getSafeStatus :: GetSafeStatus m
    , deploySafe :: DeploySafe m
    , deployToken :: DeployToken m
    , addTrustConnection :: AddTrustConnection m
    }
