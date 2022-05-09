module CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing where

import Prelude
import CirclesCore (ApiError, NativeError)
import CirclesPink.Garden.StateMachine.Control.Env (RequestPath, UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask)
import Data.Argonaut (JsonDecodeError)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData)
import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- Transitions
--------------------------------------------------------------------------------
type LandingTransitions
  = ( signUp :: P.Action ("infoGeneral" :> Nil')
    , signIn :: P.Action ("login" :> Nil')
    , checkForSession :: P.Action ("landing" :> "trusts" :> "dashboard" :> Nil')
    )

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------
type LandingState
  = { checkSessionResult :: LandingStateCheckSessionResult
    }

type LandingStateCheckSessionResult
  = RemoteData Unit Unit ErrLandingStateResolved Unit

--------------------------------------------------------------------------------
-- Action
--------------------------------------------------------------------------------
type LandingAction
  = Variant
      ( signIn :: Unit
      , signUp :: Unit
      , checkForSession :: Unit
      )

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------
type ErrLandingStateResolved
  = Variant
      ( errDecode :: JsonDecodeError
      , errReadStorage :: RequestPath
      , errApi :: ApiError
      , errNative :: NativeError
      , errUserNotFound :: UserNotFoundError
      , errInvalidUrl :: String
      )

type ErrLandingState
  = Env.ErrRestoreSession
      + ErrLoginTask
      + ()

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
_landing :: forall a v. a -> Variant ( landing :: a | v )
_landing = inj (Proxy :: _ "landing")
