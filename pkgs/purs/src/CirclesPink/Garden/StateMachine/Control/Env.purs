module CirclesPink.Garden.StateMachine.Control.Env
  ( Env
  , EnvApiCheckEmail
  , EnvApiCheckUserName
  , EnvGetSafeAddress
  , GetSafeAddressError
  , PrepareSafeDeployError
  , RegisterError
  , UserNotFoundError
  , UserResolveError
  ) where

import Prelude
import CirclesCore (UserOptions, User, ApiError)
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Effect.Exception (Error)
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey)

type RegisterError r
  = ( errService :: Unit, errNative :: Error | r )

type GetSafeAddressError r
  = ( errNative :: Error | r )

type PrepareSafeDeployError r
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

type EnvApiCheckUserName m
  = String -> ExceptT CirclesError m { isValid :: Boolean }

type EnvApiCheckEmail m
  = String -> ExceptT CirclesError m { isValid :: Boolean }

type EnvGetSafeAddress m
  = forall r. PrivateKey -> ExceptV (GetSafeAddressError + r) m Address

type Env m
  = { apiCheckUserName :: EnvApiCheckUserName m
    , apiCheckEmail :: EnvApiCheckEmail m
    , generatePrivateKey :: m PrivateKey
    , userRegister :: forall r. PrivateKey -> UserOptions -> ExceptV (RegisterError + r) m Unit
    , getSafeAddress :: EnvGetSafeAddress m
    , safePrepareDeploy :: forall r. PrivateKey -> ExceptV (PrepareSafeDeployError + r) m Address
    , userResolve :: forall r. PrivateKey -> ExceptV (UserResolveError + r) m User
    }
