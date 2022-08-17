module CirclesPink.Garden.StateMachine.Stories
  -- ( ScriptT'
  -- , SignUpUserOpts
  -- , execScripT'
  -- , finalizeAccount
  -- , loginUser
  -- , signUpUser
  -- , trustUser
  -- ) 
  where

import Prelude

import CirclesPink.Data.UserIdent (UserIdent)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Control.Monad.State (class MonadState, get)
import Data.Variant.Extra (getLabel)
import Log.Class (class MonadLog, log)
import Stadium.Control (toMonadState)

class
  ( MonadCircles m
  , MonadLog m
  , MonadState CirclesState m
  ) <=
  MonadScript m

--------------------------------------------------------------------------------
act :: forall m. MonadScript m => EnvControl m -> CirclesConfig m -> CirclesAction -> m Unit
act env cfg =
  let
    ctl ac = toMonadState (circlesControl env cfg) ac
  in
    \ac -> do
      log ("ACTION: " <> show ac)
      ctl ac
      st <- get
      log ("STATE: " <> getLabel st)
      log ""
      pure unit

--------------------------------------------------------------------------------
type SignUpUserOpts =
  { username :: String
  , email :: String
  }

signUpUser :: forall m. MonadScript m => EnvControl m -> CirclesConfig m -> SignUpUserOpts -> m Unit
signUpUser env cfg opts = do
  act env cfg $ A._landing $ A._signUp unit
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
finalizeAccount :: forall m. MonadScript m => EnvControl m -> CirclesConfig m -> m Unit
finalizeAccount env cfg = do
  act env cfg $ A._trusts $ A._finalizeRegisterUser unit

--------------------------------------------------------------------------------
type LoginUserOpts =
  { magicWords :: String
  }

loginUser :: forall m. MonadScript m => EnvControl m -> CirclesConfig m -> LoginUserOpts -> m Unit
loginUser env cfg { magicWords } = do
  act env cfg $ A._landing $ A._signIn unit
  act env cfg $ A._login $ A._setMagicWords magicWords
  act env cfg $ A._login $ A._login unit

--------------------------------------------------------------------------------
type TrustUserOpts = UserIdent

trustUser :: forall m. MonadScript m => EnvControl m -> CirclesConfig m -> TrustUserOpts -> m Unit
trustUser env cfg u = do
  act env cfg $ A._dashboard $ A._addTrustConnection u
