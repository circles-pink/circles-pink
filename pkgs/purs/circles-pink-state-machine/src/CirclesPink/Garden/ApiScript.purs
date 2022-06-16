module CirclesPink.Garden.ApiScript where

import Prelude

import Chance as C
import CirclesCore (ErrSendTransaction, ErrNewWebSocketProvider)
import CirclesPink.EnvVars (EnvVars, getParsedEnv)
import CirclesPink.Garden.Env (env, liftEnv)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing (initLanding)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (SignUpUserOpts)
import CirclesPink.Garden.StateMachine.Stories as S
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, throwError)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.State (StateT, get, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Convertable (convert)
import Data.Argonaut (decodeJson, encodeJson, fromString, stringify)
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype.Extra ((-|))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, default, inj, onMatch)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import HTTP.Milkis (milkisRequest)
import Log.Class (class MonadLog)
import Milkis.Impl.Node (nodeFetch)
import Network.Ethereum.Core.Signatures as W3
import Network.Ethereum.Web3 (HexString)
import Node.ChildProcess (defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Process (exit)
import Partial.Unsafe (unsafePartial)
import Record as R
import Sunde (spawn)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey, getWords, keyToMnemonic)
import Wallet.PrivateKey as CC
import Web3 (newWeb3, newWebSocketProvider, sendTransaction)

--------------------------------------------------------------------------------
type ScriptM e a = ScriptT e Aff a

type ScriptT e m a = ExceptV e (StateT CirclesState m) a

type ErrApp r = Err + ErrSendTransaction + ErrNewWebSocketProvider + r

runScriptM :: forall e a. ScriptM e a -> Aff (Either (Variant e) a /\ CirclesState)
runScriptM = runScripT

runScripT :: forall e m a. ScriptT e m a -> m (Either (Variant e) a /\ CirclesState)
runScripT = flip runStateT initLanding <<< runExceptT

type Err r = (err :: String | r)

err :: forall r. String -> Variant (err :: String | r)
err = inj (Proxy :: _ "err")

--------------------------------------------------------------------------------
type AppM r a = ExceptV (ErrApp + r) Aff a

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
type MkAccountReturn =
  { username :: String
  , email :: String
  , privateKey :: PrivateKey
  , safeAddress :: Address
  , txHash :: HexString
  , mnemonic :: String
  }

--------------------------------------------------------------------------------
type SignUpUser =
  { privateKey :: CC.PrivateKey
  , safeAddress :: CC.Address
  }

signUpUser :: forall m r. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> SignUpUserOpts -> ScriptT (Err + r) m SignUpUser
signUpUser env cfg opts =
  ExceptT do
    S.signUpUser env cfg opts
    get
      <#>
        ( default (throwError $ err "Cannot sign up user.")
            # onMatch
                { trusts:
                    \x ->
                      pure
                        { privateKey: x.privKey
                        , safeAddress: x.user.safeAddress
                        }
                }
        )

finalizeAccount :: forall m r. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> ScriptT (Err + r) m Unit
finalizeAccount env cfg =
  ExceptT do
    S.finalizeAccount env cfg
    get
      <#>
        ( default (throwError $ err "Cannot finalize register user.")
            # onMatch
                { dashboard: \_ -> pure unit
                }
        )

--------------------------------------------------------------------------------
mkAccount :: forall r. EnvVars -> Env (StateT CirclesState Aff) -> CirclesConfig (StateT CirclesState Aff) -> ScriptM (ErrApp + r) MkAccountReturn
mkAccount envVars env cfg = do
  signupOpts <- genSignupOpts
  { privateKey, safeAddress } <- signUpUser env cfg signupOpts
  txHash <- fundAddress envVars $ convert safeAddress
  finalizeAccount env cfg
  let
    mnemonic = keyToMnemonic privateKey
  pure
    $ R.merge signupOpts
        { privateKey
        , safeAddress
        , txHash
        , mnemonic: getWords mnemonic # joinWith " "
        }

--------------------------------------------------------------------------------
app :: forall r. EnvVars -> Env (StateT CirclesState Aff) -> CirclesConfig (StateT CirclesState Aff) -> AppM (ErrApp + r) Unit
app ev env cfg = do
  let
    count = 25

    maxPar = 5

    reportFilePath = "account-report.json"

    jqPath = "jq"

    batchCount = floor (toNumber count / toNumber maxPar)
  report <-
    (parTraverse (const mkAccount') (1 .. min maxPar count))
      # (\m -> traverse (const m) (1 .. batchCount))
      <#> join
      <#> map (lmap printErrApp)
      # lift
  report
    # encodeJson
    # stringify
    # (\r -> do lift $ spawn { cmd: jqPath, args: [], stdin: pure r } defaultSpawnOptions)
    >>= (\{ stdout } -> lift $ writeTextFile UTF8 reportFilePath stdout)
  _ <- liftEffect $ exit 0
  pure unit
  where
  mkAccount' :: Aff (Either (Variant (ErrApp + r)) MkAccountReturn)
  mkAccount' = mkAccount ev env cfg # runScriptM <#> fst

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
    Left err' -> do
      error ("ERROR: " <> show err')
      exit 1
    Right envVars -> do
      let
        request = milkisRequest nodeFetch

        env'' = liftEnv $ env { envVars: convert envVars, request }

        cfg = CirclesConfig { extractEmail : Nothing  }
      runAppM $ app envVars env'' cfg
