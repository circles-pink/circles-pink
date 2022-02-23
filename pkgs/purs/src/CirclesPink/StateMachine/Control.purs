module CirclesPink.StateMachine.Control
  ( Env
  , circlesControl
  ) where

import Prelude
import CirclesPink.StateMachine (_circlesStateMachine)
import CirclesPink.StateMachine.Action (CirclesAction)
import CirclesPink.StateMachine.State (CirclesState)
import CirclesPink.StateMachine.State as S
import Stadium.Control as C

type Env m
  = { apiCheckUserName :: String -> m Boolean
    , apiCheckEmail :: String -> m Boolean
    }

circlesControl :: forall m. Monad m => Env m -> ((CirclesState -> CirclesState) -> m Unit) -> CirclesState -> CirclesAction -> m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { infoGeneral:
        { next: \set _ _ -> set $ \st -> S._askUsername st }
    , askUsername:
        { prev: \set _ _ -> set $ \st -> S._infoGeneral st
        , setUsername: \set _ x -> set $ \st -> S._askUsername st { username = x }
        }
    }
