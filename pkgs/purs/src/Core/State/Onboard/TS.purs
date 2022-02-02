module Core.State.Onboard.TS where

import Prelude
import Core.State.Onboard (Env, Msg, State)
import Effect.Promise (Promise)
import Undefined (undefined)

reducerPromise :: Env Promise -> Msg -> State -> Promise State
reducerPromise = undefined
