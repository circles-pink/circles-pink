module CirclesPink.Garden.StateMachine.ProtocolDef
  ( CirclesProtocolDef
  , ErrLandingState
  , ErrLandingStateResolved
  , GetAction
  , GetProtocol
  , GetState
  , LandingAction
  , LandingState
  , LandingStateCheckSessionResult
  , LandingTransitions
  , _landing
  ) where

import Prelude
import Prelude
import CirclesCore (ApiError, NativeError)
import CirclesPink.Garden.StateMachine.Control.Env (RequestPath, UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask)
import Data.Argonaut (JsonDecodeError)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData(..))
import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- CirclesProtocol'
--------------------------------------------------------------------------------
type CirclesProtocolDef f
  = ( landing :: f (P.State LandingTransitions) LandingState LandingAction
    -- , infoGeneral :: f (P.State LandingTransitions) LandingState LandingAction
    -- , askUsername :: f (P.State LandingTransitions) LandingState LandingAction
    -- , askEmail :: f (P.State LandingTransitions) LandingState LandingAction
    -- , infoSecurity :: f (P.State LandingTransitions) LandingState LandingAction
    -- , magicWords :: f (P.State LandingTransitions) LandingState LandingAction
    -- , submit :: f (P.State LandingTransitions) LandingState LandingAction
    -- , dashboard :: f (P.State LandingTransitions) LandingState LandingAction
    -- , login :: f (P.State LandingTransitions) LandingState LandingAction
    -- , trusts :: f (P.State LandingTransitions) LandingState LandingAction
    -- , debug :: f (P.State LandingTransitions) LandingState LandingAction
    )

--------------------------------------------------------------------------------
-- Landing
--------------------------------------------------------------------------------
type LandingTransitions
  = ( signUp :: P.Action ("infoGeneral" :> Nil')
    , signIn :: P.Action ("login" :> Nil')
    , checkForSession :: P.Action ("landing" :> "trusts" :> "dashboard" :> Nil')
    )

type LandingState
  = { checkSessionResult :: LandingStateCheckSessionResult
    }

type LandingAction
  = Variant
      ( signIn :: Unit
      , signUp :: Unit
      , checkForSession :: Unit
      )

_landing :: forall a v. a -> Variant ( landing :: a | v )
_landing = inj (Proxy :: _ "landing")

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

type LandingStateCheckSessionResult
  = RemoteData Unit Unit ErrLandingStateResolved Unit

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type GetProtocol p s a
  = p

type GetState p s a
  = s

type GetAction p s a
  = a
