module CirclesPink.Garden.ApiScript where

import Prelude
import CirclesPink.Garden.Env (env)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (Env, circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState, init)
import Control.Monad.State (StateT(..), execState, execStateT)
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)
import Stadium.Control (toStateT)
import Undefined (undefined)

--act :: CirclesAction -> StateT CirclesState Aff Unit
control ::
  ( (CirclesState -> CirclesState) ->
    StateT CirclesState Aff Unit
  ) ->
  CirclesState -> CirclesAction -> StateT CirclesState Aff Unit
control =
  -- milkisRequest windowFetch
  -- # (\request -> env { request })
  -- # 
  (undefined :: Env (StateT CirclesState Aff))
    # circlesControl

act :: CirclesAction -> StateT CirclesState Aff Unit
act = toStateT control

script :: StateT CirclesState Aff Unit
script = do
  act (A._infoGeneral $ A._next unit)

result :: Aff CirclesState
result = execStateT script init

main :: Effect Unit
main = do
  log "hello script"
  runAff_ (\_ -> pure unit) result
