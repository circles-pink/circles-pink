module CirclesPink.Garden.ApiScript
  ( main
  ) where

import Prelude
import Chance as C
import CirclesPink.EnvVars (getParsedEnv)
import CirclesPink.Garden.Env (EnvVars, env)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (ScriptT, runScripT)
import CirclesPink.Garden.StateMachine.Stories as S
import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import HTTP (addLogging)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Node.Process (exit)

control ::
  EnvVars ->
  ((CirclesState -> CirclesState) -> StateT CirclesState Aff Unit) ->
  CirclesState -> CirclesAction -> StateT CirclesState Aff Unit
control envVars =
  milkisRequest nodeFetch
    # addLogging
    # (\request -> env { request, envVars })
    # circlesControl

app :: Env Aff -> ScriptT Aff Unit
app env = do
  pk <- S.signUpUser env { username: "Foo1", email: "foo1@bar.com" }
  --fundAddress pk
  S.finalizeAccount env
  pure unit

main :: Effect Unit
main = do
  name <- C.name {}
  log name
  config <- getParsedEnv
  case config of
    Left err -> do
      log ("ERROR: " <> show err)
      exit 1
    Right val -> do
      runAff_ (const $ pure unit) $ runScripT $ app undefined
  log "hello api script"
