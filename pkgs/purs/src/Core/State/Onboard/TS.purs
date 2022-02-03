module Core.State.Onboard.TS where

import Prelude
import Core.State.Onboard (Env, Msg, State)
import Effect.Promise (Promise)
import Undefined (undefined)

reducerPromise :: Env Promise -> (State -> Promise Unit) -> Msg -> State -> Promise Unit
reducerPromise = undefined
