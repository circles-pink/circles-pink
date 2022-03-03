module CirclesPink.Garden.ApiScript where

import Prelude
import CirclesPink.Garden.Env (env)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (Env, circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState, init)
import Control.Monad.State (StateT(..), execState, execStateT, get, lift)
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Stadium.Control (toStateT)
import Undefined (undefined)

control ::
  ((CirclesState -> CirclesState) -> StateT CirclesState Aff Unit) ->
  CirclesState -> CirclesAction -> StateT CirclesState Aff Unit
control =
  milkisRequest nodeFetch
    # (\request -> env { request })
    # circlesControl

act :: CirclesAction -> StateT CirclesState Aff Unit
act = toStateT control

script :: StateT CirclesState Aff Unit
script = do
  logState
  act $ A._infoGeneral $ A._next unit
  logState
  act $ A._askUsername $ A._setUsername "fooo"
  logState
  act $ A._askUsername $ A._next unit
  logState
  act $ A._askEmail $ A._setEmail "helloworld@helloworld.com"
  act $ A._askEmail $ A._setTerms unit
  act $ A._askEmail $ A._setPrivacy unit
  logState
  act $ A._askEmail $ A._next unit
  logState
  act $ A._infoSecurity $ A._next unit
  logState
  where
  logState = get >>= (logShow >>> lift)

result :: Aff CirclesState
result = execStateT script init

main :: Effect Unit
main = do
  runAff_ (\_ -> pure unit) result
