module CirclesPink.Garden.StateMachine.Control.Env
  ( CoreToWindowError
  , Env
  , EnvApiCheckEmail
  , EnvApiCheckUserName
  , EnvCoreToWindow
  , EnvDeploySafe
  , EnvDeploySafeError
  , EnvDeployToken
  , EnvDeployTokenError
  , EnvGetSafeAddress
  , EnvGetSafeStatus
  , EnvGetSafeStatusError
  , EnvIsTrusted
  , EnvIsTrustedError
  , EnvTrustGetNetwork
  , EnvTrustGetNetworkError
  , GetSafeAddressError
  , PrepareSafeDeployError
  , RegisterError
  , UserNotFoundError
  , UserResolveError
  ) where

import Prelude
import CirclesCore (ApiError, ErrInvalidUrl, ErrNative, ErrService, TrustIsTrustedResult, TrustNode, User, UserOptions, SafeStatus)
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey)

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------
type RegisterError r
  = ErrService + ErrNative + ErrInvalidUrl + r

type GetSafeAddressError r
  = ErrNative + ErrInvalidUrl + r

type PrepareSafeDeployError r
  = ErrNative + ErrInvalidUrl + r

type CoreToWindowError r
  = ErrNative + ErrInvalidUrl + r

type EnvIsTrustedError r
  = ErrNative + ErrInvalidUrl + r

type EnvTrustGetNetworkError r
  = ErrNative + ErrInvalidUrl + r

type EnvGetSafeStatusError r
  = ErrNative + ErrInvalidUrl + r

type UserNotFoundError
  = { safeAddress :: Address
    }

type UserResolveError r
  = ErrNative + ErrInvalidUrl
      + ( errApi :: ApiError
      , errUserNotFound :: UserNotFoundError
      | r
      )

type EnvDeploySafeError r
  = ErrService + ErrInvalidUrl + ErrNative + r

type EnvDeployTokenError r
  = ErrService + ErrInvalidUrl + ErrNative + r

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------
type EnvApiCheckUserName m
  = String -> ExceptT CirclesError m { isValid :: Boolean }

type EnvApiCheckEmail m
  = String -> ExceptT CirclesError m { isValid :: Boolean }

type EnvGetSafeAddress m
  = forall r. PrivateKey -> ExceptV (GetSafeAddressError + r) m Address

type EnvCoreToWindow m
  = forall r. PrivateKey -> ExceptV (CoreToWindowError + r) m Unit

type EnvIsTrusted m
  = forall r. PrivateKey -> ExceptV (EnvIsTrustedError + r) m TrustIsTrustedResult

type EnvTrustGetNetwork m
  = forall r. PrivateKey -> ExceptV (EnvTrustGetNetworkError + r) m (Array TrustNode)

type EnvGetSafeStatus m
  = forall r. PrivateKey -> ExceptV (EnvGetSafeStatusError + r) m SafeStatus

type EnvDeploySafe m
  = forall r. PrivateKey -> ExceptV (EnvDeploySafeError + r) m Unit

type EnvDeployToken m
  = forall r. PrivateKey -> ExceptV (EnvDeployTokenError + r) m String

type Env m
  = { apiCheckUserName :: EnvApiCheckUserName m
    , apiCheckEmail :: EnvApiCheckEmail m
    , generatePrivateKey :: m PrivateKey
    , userRegister :: forall r. PrivateKey -> UserOptions -> ExceptV (RegisterError + r) m Unit
    , getSafeAddress :: EnvGetSafeAddress m
    , safePrepareDeploy :: forall r. PrivateKey -> ExceptV (PrepareSafeDeployError + r) m Address
    , userResolve :: forall r. PrivateKey -> ExceptV (UserResolveError + r) m User
    , coreToWindow :: EnvCoreToWindow m
    , isTrusted :: EnvIsTrusted m
    , trustGetNetwork :: EnvTrustGetNetwork m
    , getSafeStatus :: EnvGetSafeStatus m
    , deploySafe :: EnvDeploySafe m
    , deployToken :: EnvDeployToken m
    }
