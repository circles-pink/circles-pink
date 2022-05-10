module CirclesPink.Garden.ApiScript
  ( main
  ) where

import Prelude
import Chance as C
import CirclesPink.EnvVars (getParsedEnv)
import CirclesPink.Garden.Env (env)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.Stories (ScriptT, runScripT)
import CirclesPink.Garden.StateMachine.Stories as S
import Convertable (convert)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log, logShow)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Node.Process (exit)

app :: Env Aff -> ScriptT Aff Unit
app env = do
  pk <- S.signUpUser env { username: "Foo1", email: "foo1@bar.com" }
  --fundAddress pk
  --S.finalizeAccount env
  pure unit

main :: Effect Unit
main = do
  name <- C.name {}
  log name
  envVars' <- getParsedEnv
  case envVars' of
    Left err -> do
      log ("ERROR: " <> show err)
      exit 1
    Right envVars -> do
      let
        request = milkisRequest nodeFetch

        env' = env { envVars: convert envVars, request }
      runAff_
        ( \r -> do
            case r of
              Left e -> logShow e
              Right _ -> pure unit
        )
        $ runScripT
        $ app env'
  log "hello api script"
