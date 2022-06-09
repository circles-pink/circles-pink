module CirclesPink.Garden.StateMachine.State.Login where

import Prelude

import CirclesCore (ApiError, NativeError)
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData, _notAsked)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- Login State
--------------------------------------------------------------------------------

type LoginState =
  { magicWords :: String
  , loginResult :: LoginStateLoginResult
  }

type ErrLoginState = ErrLoginTask
  + Env.ErrSaveSession
  + ()

type ErrLoginStateResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errUserNotFound :: UserNotFoundError
  , errInvalidUrl :: String
  , errSaveSession :: Unit
  , errInvalidMnemonic :: Unit
  )

type LoginStateLoginResult = RemoteData Unit Unit ErrLoginStateResolved Unit

--------------------------------------------------------------------------------
-- InitLogin
--------------------------------------------------------------------------------

initLogin :: forall v. Variant (login :: LoginState | v)
initLogin =
  _login
    { magicWords: ""
    , loginResult: _notAsked unit
    }

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

_login :: forall a v. a -> Variant (login :: a | v)
_login = inj (Proxy :: _ "login")
