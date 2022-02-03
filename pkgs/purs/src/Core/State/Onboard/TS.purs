module Core.State.Onboard.TS where

import Prelude
import Core.State.Onboard (Env, Msg, State)
import Core.State.Onboard as O
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Undefined (undefined)

reducerAff :: Env Aff -> (State -> Aff Unit) -> Msg -> State -> Effect Unit
reducerAff e f m s = O.reducer e f m s # launchAff_
