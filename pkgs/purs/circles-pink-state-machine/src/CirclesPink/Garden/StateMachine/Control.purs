module CirclesPink.Garden.StateMachine.Control (circlesControl) where

import Prelude

import CirclesPink.Garden.StateMachine.Type (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Control.States as States
import CirclesPink.Garden.StateMachine.Control.States.AskEmail as States.AskEmail

import CirclesPink.Garden.StateMachine.State as S
import Stadium.Control as C

circlesControl
  :: forall m
   . MonadCircles m
  => EnvControl m
  -> CirclesConfig m
  -> ((S.CirclesState -> S.CirclesState) -> m Unit)
  -> S.CirclesState
  -> CirclesAction
  -> m Unit
circlesControl env cfg =
  C.mkControl
    _circlesStateMachine
    { landing: States.landing $ env
    , askUsername: States.askUsername env cfg
    , askEmail
    , infoSecurity: States.infoSecurity env
    , magicWords: States.magicWords env
    , submit: States.submit env
    , dashboard: States.dashboard env
    , login: States.login env
    , trusts: States.trusts env
    , debug: States.debug env
    }
  where

  askEmail =
    { prev:
        States.AskEmail.prev env
    , setTerms:
        States.AskEmail.setTerms env
    , setPrivacy:
        States.AskEmail.setPrivacy env
    , next:
        States.AskEmail.next env
    , setEmail:
        States.AskEmail.setEmail env
    }
