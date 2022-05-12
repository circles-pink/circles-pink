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
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (Err, ScriptT, SignUpUserOpts, finalizeAccount, runScripT, signUpUser)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Convertable (convert)
import Data.Argonaut (decodeJson, encodeJson, fromString, stringify)
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Int (floor, toNumber)
import Data.Maybe (fromJust)
import Data.Newtype.Extra ((-|))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, default, onMatch)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
import Network.Ethereum.Core.Signatures as W3
import Network.Ethereum.Web3 (HexString)
import Node.Process (exit)
import Partial.Unsafe (unsafePartial)
import Record as R
import Type.Row (type (+))
import Wallet.PrivateKey (Address, Mnemonic, PrivateKey, keyToMnemonic)
import Web3 (newWeb3, newWebSocketProvider, sendTransaction)

--------------------------------------------------------------------------------
type ScriptM e a
  = ScriptT e Aff a

type ErrApp r
  = Err + ErrSendTransaction + ErrNewWebSocketProvider + r

runScriptM :: forall e a. ScriptM e a -> Aff (Either (Variant e) a /\ CirclesState)
runScriptM = runScripT

--------------------------------------------------------------------------------
type AppM r a
  = ExceptV (ErrApp + r) Aff a

runAppM :: forall r a. AppM r a -> Effect Unit
runAppM x =
  x
    # runExceptT
    # runAff_
        ( case _ of
            Left e -> log ("Native error: " <> show e)
            Right (Left e) -> log ("Error: " <> printErrApp e)
            _ -> pure unit
        )

--------------------------------------------------------------------------------
safeFunderAddr :: W3.Address
safeFunderAddr =
  unsafePartial
    ( "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1"
        # fromString
        # decodeJson
        # hush
        # fromJust
    )

--------------------------------------------------------------------------------
fundAddress :: forall r. EnvVars -> W3.Address -> ScriptM (ErrApp + r) HexString
fundAddress envVars safeAddress =
  mapExceptT lift do
    provider <- newWebSocketProvider $ envVars -| _.gardenEthereumNodeWebSocket
    web3 <- lift $ newWeb3 provider
    sendTransaction web3
      { from: safeFunderAddr
      , to: safeAddress
      , value: "1000000000000000000"
      }

--------------------------------------------------------------------------------
genUsername :: forall r. ScriptM r String
genUsername =
  (lift <<< lift) ado
    n <- C.first {}
    i <- C.integer { min: 1, max: 99 }
    in n <> show i

--------------------------------------------------------------------------------
genSignupOpts :: forall r. ScriptM r SignUpUserOpts
genSignupOpts = do
  username <- genUsername
  email <- pure (username <> "@bar.com")
  pure { username, email }

--------------------------------------------------------------------------------
type MkAccountReturn
  = { username :: String
    , email :: String
    , privateKey :: PrivateKey
    , safeAddress :: Address
    , txHash :: HexString
    , mnemonic :: Mnemonic
    }

mkAccount :: forall r. EnvVars -> Env Aff -> ScriptM (ErrApp + r) MkAccountReturn
mkAccount envVars env = do
  signupOpts <- genSignupOpts
  { privateKey, safeAddress } <- signUpUser env signupOpts
  txHash <- fundAddress envVars $ convert safeAddress
  finalizeAccount env
  let
    mnemonic = keyToMnemonic privateKey
  pure $ R.merge signupOpts { privateKey, safeAddress, txHash, mnemonic }

--------------------------------------------------------------------------------
app :: forall r. EnvVars -> Env Aff -> AppM (ErrApp + r) Unit
app ev env = do
  let
    count = 100

    maxPar = 10

    batchCount = floor (toNumber count / toNumber maxPar)
  report <-
    (parTraverse (const mkAccount') (1 .. maxPar))
      # (\m -> traverse (const m) (1 .. batchCount))
      <#> join
      <#> map (lmap printErrApp)
      # lift
  report # encodeJson # stringify # log
  pure unit
  where
  mkAccount' :: Aff (Either (Variant (ErrApp + r)) MkAccountReturn)
  mkAccount' = mkAccount ev env # runScriptM <#> fst

--------------------------------------------------------------------------------
printErrApp :: forall r. Variant (ErrApp + r) -> String
printErrApp =
  default "Unknown Error"
    # onMatch
        { err: \str -> str
        }

--------------------------------------------------------------------------------
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
      runAppM $ app envVars env'
