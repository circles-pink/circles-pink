module CirclesPink.TS where

import Prelude
import CirclesPink.StateMachine.Action (CirclesAction)
import CirclesPink.StateMachine.Control (Env, circlesControl)
import CirclesPink.StateMachine.State (CirclesState)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Undefined (undefined)

--- type StateMachine = (s -> Effect Unit) -> m -> s -> Effect Unit
circlesControlEff :: Env Aff -> (CirclesState -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
circlesControlEff e f s a = circlesControl e (liftEffect <<< f) s a # launchAff_
