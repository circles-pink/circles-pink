module CirclesPink.Garden.StateMachine.State.Login where

import Prelude

import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Garden.StateMachine.Control.Common (TaskReturn, ErrLoginTask)
import CirclesPink.Garden.StateMachine.Control.EnvControl as Env
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, inj)
import RemoteData (_notAsked)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- Login State
--------------------------------------------------------------------------------

type LoginState =
  { magicWords :: String
  , loginResult :: LoginStateLoginResult
  }

type ErrLoginState r = ErrLoginTask
  + Env.ErrSaveSession
  + Env.ErrInvalidMnemonic
  + r

type LoginStateLoginResult = RemoteReport
  (Variant (ErrLoginState + ()))
  (TaskReturn /\ PrivateKey)

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
