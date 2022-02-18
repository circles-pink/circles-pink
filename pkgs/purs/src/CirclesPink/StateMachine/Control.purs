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

circlesControl :: forall m. Monad m => (CirclesState -> m Unit) -> CirclesState -> CirclesAction -> m Unit
circlesControl =
  C.mkControl
    _circlesStateMachine
    { infoGeneral:
        { next: \set st _ -> set $ S._askUsername st }
    , askUsername:
        { prev: \set st _ -> set $ S._infoGeneral st
        , setUsername: \set st x -> set $ S._askUsername st { username = x }
        }
    }
