module CirclesPink.Garden.ApiScript where

import Prelude

import Chance as C
import CirclesCore (ErrNewWebSocketProvider, ErrSendTransaction)
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.Mnemonic (getWords, keyToMnemonic)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.EnvVars (EnvVars, getParsedEnv)
import CirclesPink.Garden.Env (env)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control.Class.ScriptM (ScriptM, evalScriptM)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.Stories (SignUpUserOpts)
import CirclesPink.Garden.StateMachine.Stories as S
import CirclesPink.Garden.TestEnv (liftEnv)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, throwError)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (class Parallel, parTraverse)
import Convertable (convert)
import Data.Argonaut (decodeJson, encodeJson, fromString, stringify)
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Int (floor, toNumber)
import Data.Maybe (fromJust)
import Data.Newtype.Extra ((-|))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Variant (Variant, default, inj, onMatch)
import Effect (Effect)
import Effect.Aff (Aff, message, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Node (nodeFetch)
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
import Web3 (newWeb3, newWebSocketProvider, sendTransaction)

type ErrApp r = Err + ErrSendTransaction + ErrNewWebSocketProvider + r

type Err r = (err :: String | r)

err :: forall r. String -> Variant (err :: String | r)
err = inj (Proxy :: _ "err")

--------------------------------------------------------------------------------
safeFunderAddr :: Address
safeFunderAddr =
  unsafePartial
    ( "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1"
        # fromString
        # decodeJson
        # hush
        # fromJust
    )

--------------------------------------------------------------------------------
fundAddress :: forall r. EnvVars -> Address -> ExceptV (ErrApp + r) ScriptM HexString
fundAddress envVars safeAddress =
  mapExceptT liftAff do
    provider <- newWebSocketProvider $ envVars -| _.gardenEthereumNodeWebSocket
    web3 <- lift $ newWeb3 provider
    sendTransaction web3
      { from: safeFunderAddr
      , to: safeAddress
      , value: "1000000000000000000"
      }

--------------------------------------------------------------------------------
genUsername :: forall r. ExceptV r ScriptM String
genUsername =
  liftAff ado
    n <- C.first {}
    i <- C.integer { min: 1, max: 99 }
    in n <> show i

--------------------------------------------------------------------------------
genSignupOpts :: forall r. ExceptV r ScriptM SignUpUserOpts
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
  { privateKey :: PrivateKey
  , safeAddress :: Address
  }

signUpUser :: forall r. Env ScriptM -> CirclesConfig ScriptM -> SignUpUserOpts -> ExceptV (Err + r) ScriptM SignUpUser
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

finalizeAccount :: forall r. Env ScriptM -> CirclesConfig ScriptM -> ExceptV (Err + r) ScriptM Unit
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
mkAccount :: forall r. EnvVars -> Env ScriptM -> CirclesConfig ScriptM -> ExceptV (ErrApp + r) ScriptM MkAccountReturn
mkAccount envVars env cfg = do
  signupOpts <- genSignupOpts
  { privateKey, safeAddress } <- signUpUser env cfg signupOpts
  txHash <- fundAddress envVars safeAddress
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

traverse' :: forall m p a. Applicative m => Parallel p m => { count :: Int, maxPar :: Int } -> m a -> m (Array a)
traverse' { maxPar, count } f = parTraverse (const sequential) (1 .. parCount) <#> join
  where
  sequential = traverse (const f) (1 .. batchCount)
  parCount = min maxPar count
  batchCount = floor (toNumber count / toNumber maxPar)

--------------------------------------------------------------------------------
printErrApp :: forall r. Variant (ErrApp + r) -> String
printErrApp =
  default "Unknown Error"
    # onMatch
        { err: \str -> str
        }

--------------------------------------------------------------------------------

reportFilePath :: String
reportFilePath = "account-report.json"

main' :: ExceptT String Aff Unit
main' = do
  envVars <- ExceptT $ map (lmap show) $ liftEffect getParsedEnv
  let
    request = milkisRequest nodeFetch
    env'' = liftEnv liftAff $ env { envVars: convert envVars, request }
    cfg = CirclesConfig { extractEmail: Right (\_ -> pure unit) }

  (mkAccount envVars env'' cfg # runExceptT # evalScriptM)
    # traverse' { count: 20, maxPar: 5 }
    <#> (map (lmap printErrApp) >>> encodeJson >>> stringify)
    >>= runJq
    >>= writeTextFile UTF8 reportFilePath
    # lift

main :: Effect Unit
main = runExceptT main' # runAff_ handler
  where
  handler (Left errNat) = do
    error ("UNKNOWN ERROR: " <> message errNat)
    exit 1
  handler (Right (Left err')) = do
    error ("ERROR: " <> err')
    exit 1
  handler (Right (Right _)) = pure unit

runJq :: String -> Aff String
runJq str = spawn { cmd: "jq", args: [], stdin: pure str } defaultSpawnOptions
  <#> _.stdout
