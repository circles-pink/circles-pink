module CirclesPink.Garden.StateMachine.ProtocolDef where

import Prelude
import CirclesCore (ApiError, NativeError)
import CirclesPink.Garden.StateMachine.Control.Env (RequestPath, UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask)
import Data.Argonaut (JsonDecodeError)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData(..), _notAsked)
import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing as Landing

--------------------------------------------------------------------------------
-- CirclesProtocolDef
--------------------------------------------------------------------------------
type CirclesProtocolDef f
  = ( landing :: f (P.State Landing.LandingTransitions) Landing.LandingState Landing.LandingAction
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
-- Utils
--------------------------------------------------------------------------------
type GetProtocol p s a
  = p

type GetState p s a
  = s

type GetAction p s a
  = a
