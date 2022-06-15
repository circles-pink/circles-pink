module CirclesPink.Garden.StateMachine.Stories
  ( ScriptT'
  , SignUpUserOpts
  , execScripT'
  , finalizeAccount
  , loginUser
  , signUpUser
  , trustUser
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing (initLanding)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Control.Monad.State (StateT, execStateT, get)
import Data.Variant.Extra (getLabel)
import Log.Class (class MonadLog, log)
import Network.Ethereum.Core.Signatures as W3
import Stadium.Control (toStateT)
import Wallet.PrivateKey as CC

type ScriptT' m a
  = (StateT CirclesState m) a

execScripT' :: forall m a. Monad m => ScriptT' m a -> m CirclesState
execScripT' = flip execStateT initLanding

--------------------------------------------------------------------------------
act :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesAction -> ScriptT' m Unit
act env =
  let
    ctl = toStateT (circlesControl env)
  in
    \ac -> do
      log ("ACTION: " <> show ac)
      ctl ac
      st <- get
      log ("STATE: " <> getLabel st)
      log ""

--------------------------------------------------------------------------------
type SignUpUserOpts
  = { username :: String
    , email :: String
    }

signUpUser :: forall m. MonadLog m => Env (StateT CirclesState m) -> SignUpUserOpts -> ScriptT' m Unit
signUpUser env opts = do
  act env $ A._landing $ A._signUp unit
  act env $ A._infoGeneral $ A._next unit
  act env $ A._askUsername $ A._setUsername opts.username
  act env $ A._askUsername $ A._next unit
  act env $ A._askEmail $ A._setEmail opts.email
  act env $ A._askEmail $ A._setTerms unit
  act env $ A._askEmail $ A._setPrivacy unit
  act env $ A._askEmail $ A._next unit
  act env $ A._infoSecurity $ A._next unit
  act env $ A._magicWords $ A._next unit
  act env $ A._submit $ A._submit unit

--------------------------------------------------------------------------------
finalizeAccount :: forall m. MonadLog m => Env (StateT CirclesState m) -> ScriptT' m Unit
finalizeAccount env = do
  act env $ A._trusts $ A._finalizeRegisterUser unit

--------------------------------------------------------------------------------
type LoginUserOpts
  = { magicWords :: String
    }

loginUser :: forall m. MonadLog m => Env (StateT CirclesState m) -> LoginUserOpts -> ScriptT' m Unit
loginUser env { magicWords } = do
  act env $ A._landing $ A._signIn unit
  act env $ A._login $ A._setMagicWords magicWords
  act env $ A._login $ A._login unit

--------------------------------------------------------------------------------
type TrustUserOpts
  = { avatarUrl :: String
    , id :: Int
    , safeAddress :: CC.Address
    , username :: String
    }

trustUser :: forall m. MonadLog m => Env (StateT CirclesState m) -> TrustUserOpts -> ScriptT' m Unit
trustUser env u = do
  act env $ A._dashboard $ A._addTrustConnection u
