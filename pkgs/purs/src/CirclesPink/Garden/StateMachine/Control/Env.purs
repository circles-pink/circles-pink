module CirclesPink.Garden.StateMachine.Control.Env
  ( CoreToWindowError
  , Env
  , EnvApiCheckEmail
  , EnvApiCheckUserName
  , EnvCoreToWindow
  , EnvGetSafeAddress
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
import CirclesCore (TrustNode, ApiError, User, UserOptions, TrustIsTrustedResult)
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Effect.Exception (Error)
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey)

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------
type RegisterError r
  = ( errService :: Unit, errNative :: Error | r )

type GetSafeAddressError r
  = ( errNative :: Error | r )

type PrepareSafeDeployError r
  = ( errNative :: Error | r )

type CoreToWindowError r
  = ( errNative :: Error | r )

type EnvIsTrustedError r
  = ( errNative :: Error | r )

type EnvTrustGetNetworkError r
  = ( errNative :: Error | r )

type UserNotFoundError
  = { safeAddress :: Address
    }

type UserResolveError r
  = ( errNative :: Error
    , errApi :: ApiError
    , errUserNotFound :: UserNotFoundError
    | r
    )

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
    }
