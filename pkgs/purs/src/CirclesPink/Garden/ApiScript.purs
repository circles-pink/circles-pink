module CirclesPink.Garden.ApiScript where

import Prelude
import CirclesPink.Garden.Env (EnvVars, env)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState, init)
import Control.Monad.State (StateT, execStateT, get)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import HTTP (addLogging)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Stadium.Control (toStateT)

control ::
  EnvVars ->
  ((CirclesState -> CirclesState) -> StateT CirclesState Aff Unit) ->
  CirclesState -> CirclesAction -> StateT CirclesState Aff Unit
control envVars =
  milkisRequest nodeFetch
    # addLogging
    # (\request -> env { request, envVars })
    # circlesControl

act :: EnvVars -> CirclesAction -> StateT CirclesState Aff Unit
act envVars ac = do
  log ("ACTION: " <> show ac)
  toStateT (control envVars) ac
  _ <- get
  --log ("STATE: " <> show st)
  log ""

type Options
  = { username :: String
    , email :: String
    }

script :: EnvVars -> Options -> StateT CirclesState Aff Unit
script ep opts = do
  act ep $ A._infoGeneral $ A._next unit
  act ep $ A._askUsername $ A._setUsername opts.username
  act ep $ A._askUsername $ A._next unit
  act ep $ A._askEmail $ A._setEmail opts.email
  act ep $ A._askEmail $ A._setTerms unit
  act ep $ A._askEmail $ A._setPrivacy unit
  act ep $ A._askEmail $ A._next unit
  act ep $ A._infoSecurity $ A._next unit
  act ep $ A._magicWords $ A._next unit
  act ep $ A._submit $ A._submit unit

result :: EnvVars -> Aff CirclesState
result envVars = execStateT (script envVars opts) init
  where
  opts =
    { username: "pinkie001"
    , email: "pinkie001@pinkie001.net"
    }

getEnvVars :: Effect EnvVars
getEnvVars = undefined

main :: Effect Unit
main = do
  log "hello api script"

--envVars <- getEnvVars
--runAff_ (\_ -> pure unit) (result envVars)
