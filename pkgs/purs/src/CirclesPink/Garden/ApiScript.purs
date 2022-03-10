module CirclesPink.Garden.ApiScript where

import Prelude
import CirclesPink.Garden.Env (env)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState, init)
import Control.Monad.State (StateT, execStateT, get, lift)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log, logShow)
import HTTP (addLogging)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Stadium.Control (toStateT)

control ::
  ((CirclesState -> CirclesState) -> StateT CirclesState Aff Unit) ->
  CirclesState -> CirclesAction -> StateT CirclesState Aff Unit
control =
  milkisRequest nodeFetch
    # addLogging
    # (\request -> env { request })
    # circlesControl

act :: CirclesAction -> StateT CirclesState Aff Unit
act ac = do
  log ("ACTION: " <> show ac)
  toStateT control ac
  st <- get
  log ("STATE: " <> show st)
  log ""

type Options
  = { username :: String
    , email :: String
    }

script :: Options -> StateT CirclesState Aff Unit
script opts = do
  act $ A._infoGeneral $ A._next unit
  act $ A._askUsername $ A._setUsername opts.username
  act $ A._askUsername $ A._next unit
  act $ A._askEmail $ A._setEmail opts.email
  act $ A._askEmail $ A._setTerms unit
  act $ A._askEmail $ A._setPrivacy unit
  act $ A._askEmail $ A._next unit
  act $ A._infoSecurity $ A._next unit
  act $ A._magicWords $ A._next unit
  act $ A._submit $ A._submit unit

result :: Aff CirclesState
result = execStateT (script opts) init
  where
  opts =
    { username: "pinkie001"
    , email: "pinkie001@pinkie001.net"
    }

main :: Effect Unit
main = do
  runAff_ (\_ -> pure unit) result
