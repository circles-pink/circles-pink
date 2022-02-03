module Core.State.Onboard.TS where

import Prelude
import Core.State.Onboard (Env, Msg, State)
import Core.State.Onboard as O
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Undefined (undefined)

--- type StateMachine = (s -> Effect Unit) -> m -> s -> Effect Unit
reducerAff :: Env Aff -> (State -> Effect Unit) -> Msg -> State -> Effect Unit
reducerAff e f m s = O.reducer e (liftEffect <<< f) m s # launchAff_
