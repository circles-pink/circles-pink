module CirclesPink.Garden.ApiScript
  ( fundAddress
  , main
  , safeFunderAddr
  ) where

import Prelude
import Chance as C
import CirclesCore (ErrSendTransaction, ErrNewWebSocketProvider)
import CirclesPink.EnvVars (EnvVars, getParsedEnv)
import CirclesPink.Garden.Env (env)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.Stories (ScriptT, runScripT)
import CirclesPink.Garden.StateMachine.Stories as S
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Convertable (convert)
import Data.Argonaut (decodeJson, fromString)
import Data.Either (Either(..), hush)
import Data.Maybe (fromJust)
import Data.Newtype.Extra ((-|))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Network.Ethereum.Core.Signatures as W3
import Network.Ethereum.Web3 (HexString)
import Node.Process (exit)
import Partial.Unsafe (unsafePartial)
import Type.Row (type (+))
import Undefined (undefined)
import Wallet.PrivateKey (keyToMnemonic)
import Web3 (ErrPrivKeyToAccount, newWeb3, newWebSocketProvider, privKeyToAccount, sendTransaction)

safeFunderAddr :: W3.Address
safeFunderAddr =
  unsafePartial
    ( "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1"
        # fromString
        # decodeJson
        # hush
        # fromJust
    )

type ErrFundAddress r
  = ErrSendTransaction + ErrNewWebSocketProvider + ErrPrivKeyToAccount + r

fundAddress :: forall r. EnvVars -> W3.Address -> ExceptV (ErrFundAddress + r) Aff HexString
fundAddress envVars safeAddress = do
  provider <- newWebSocketProvider $ envVars -| _.gardenEthereumNodeWebSocket
  web3 <- lift $ newWeb3 provider
  sendTransaction web3
    { from: safeFunderAddr
    , to: safeAddress
    , value: "1000000000000000000"
    }

app :: EnvVars -> Env Aff -> ScriptT Aff Unit
app envVars env = do
  username <- lift $ liftEffect $ C.stringPool { pool: "abcdefghi" }
  { privateKey, safeAddress } <- S.signUpUser env { username, email: "foo1@bar.com" }
  logShow $ keyToMnemonic privateKey
  r <- liftAff $ runExceptT $ fundAddress envVars $ convert safeAddress
  case r of
    Left _ -> log ("err: ")
    Right v -> log ("ok: " <> show v)
  S.finalizeAccount env
  pure unit

main :: Effect Unit
main = do
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
        $ app envVars env'
      pure unit
  log "hello api script"
