module Core.State.Onboard.TS where

import Prelude
import Core.State.Onboard (Env, Msg, State)
import Effect.Aff (Aff)
import Undefined (undefined)

reducerAff :: Env Aff -> (State -> Aff Unit) -> Msg -> State -> Aff Unit
reducerAff = undefined
