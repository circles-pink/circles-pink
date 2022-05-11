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
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Network.Ethereum.Web3 (HexString, Web3Error)
import Network.Ethereum.Web3 as W3
import Node.Process (exit)
import Undefined (undefined)
import Web3 (sendTransaction)

safeFunderAddr :: W3.Address
safeFunderAddr = "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1"

fundAddress :: W3.PrivateKey -> Aff (Either Web3Error HexString)
fundAddress = undefined -- sendTransaction  { from: safeFunderAddr  }

app :: Env Aff -> ScriptT Aff Unit
app env = do
  pk <- S.signUpUser env { username: "Foo1", email: "foo1@bar.com" }
  fundAddress pk
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
        ( case _ of
            Left e -> log ("Native error: " <> show e)
            Right (Left e /\ _) -> log ("Error: " <> show e)
            _ -> pure unit
        )
        $ runScripT
        $ app env'
      pure unit
  log "hello api script"
