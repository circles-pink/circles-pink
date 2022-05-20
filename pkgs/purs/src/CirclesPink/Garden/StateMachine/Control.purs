module CirclesPink.Garden.StateMachine.Control (circlesControl) where

import Prelude
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Control.States as States
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Stadium.Control as C

circlesControl
  :: forall t m
   . Monad m
  => MonadTrans t
  => Monad (t m)
  => Env.Env m
  -> ((S.CirclesState -> S.CirclesState) -> t m Unit)
  -> S.CirclesState
  -> CirclesAction
  -> t m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { landing: States.landing env
    , infoGeneral: States.infoGeneral env
    , askUsername: States.askUsername env
    , askEmail: States.askEmail env
    , infoSecurity: States.infoSecurity env
    , magicWords: States.magicWords env
    , submit: States.submit env
    , dashboard: States.dashboard env
    , login: States.login env
    , trusts: States.trusts env
    , debug: States.debug env
    }
