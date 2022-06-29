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

import CirclesCore (User)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing (initLanding)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Data.UserIdent (UserIdent)
import Control.Monad.State (StateT, execStateT, get)
import Data.Either (Either)
import Data.Variant.Extra (getLabel)
import Log.Class (class MonadLog, log)
import Stadium.Control (toStateT)
import Network.Ethereum.Core.Signatures (Address)

type ScriptT' m a = (StateT CirclesState m) a

execScripT' :: forall m a. Monad m => ScriptT' m a -> m CirclesState
execScripT' = flip execStateT initLanding

--------------------------------------------------------------------------------
act :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> CirclesAction -> ScriptT' m Unit
act env cfg =
  let
    ctl = toStateT (circlesControl env cfg)
  in
    \ac -> do
      log ("ACTION: " <> show ac)
      ctl ac
      st <- get
      log ("STATE: " <> getLabel st)
      log ""

--------------------------------------------------------------------------------
type SignUpUserOpts =
  { username :: String
  , email :: String
  }

signUpUser :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> SignUpUserOpts -> ScriptT' m Unit
signUpUser env cfg opts = do
  act env cfg $ A._landing $ A._signUp unit
  act env cfg $ A._infoGeneral $ A._next unit
  act env cfg $ A._askUsername $ A._setUsername opts.username
  act env cfg $ A._askUsername $ A._next unit
  act env cfg $ A._askEmail $ A._setEmail opts.email
  act env cfg $ A._askEmail $ A._setTerms unit
  act env cfg $ A._askEmail $ A._setPrivacy unit
  act env cfg $ A._askEmail $ A._next unit
  act env cfg $ A._infoSecurity $ A._next unit
  act env cfg $ A._magicWords $ A._next unit
  act env cfg $ A._submit $ A._submit unit

--------------------------------------------------------------------------------
finalizeAccount :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> ScriptT' m Unit
finalizeAccount env cfg = do
  act env cfg $ A._trusts $ A._finalizeRegisterUser unit

--------------------------------------------------------------------------------
type LoginUserOpts =
  { magicWords :: String
  }

loginUser :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> LoginUserOpts -> ScriptT' m Unit
loginUser env cfg { magicWords } = do
  act env cfg $ A._landing $ A._signIn unit
  act env cfg $ A._login $ A._setMagicWords magicWords
  act env cfg $ A._login $ A._login unit

--------------------------------------------------------------------------------
type TrustUserOpts = UserIdent

trustUser :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesConfig (StateT CirclesState m) -> TrustUserOpts -> ScriptT' m Unit
trustUser env cfg u = do
  act env cfg $ A._dashboard $ A._addTrustConnection u
