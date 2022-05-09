module CirclesPink.Garden.StateMachine.Stories
  ( signUpUser
  ) where

import Prelude
import CirclesPink.Garden.StateMachine.Action (CirclesAction, _askEmail, _askUsername, _infoSecurity, _magicWords, _next, _setEmail, _setPrivacy, _setTerms, _setUsername, _submit)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Control.Monad.State (StateT(..))
import Data.Traversable (sequence, sequence_, traverse)
import Stadium.Control (toStateT)
import Undefined (undefined)

act :: forall m. Monad m => Env m -> CirclesAction -> StateT CirclesState m Unit
act env ac = do
  --log ("ACTION: " <> show ac)
  --_ <- get
  toStateT (circlesControl env) ac

--log ("STATE: " <> show st)
--log ""
act' :: forall m a. Monad m => Env m -> (a -> CirclesAction) -> Array a -> StateT CirclesState m Unit
act' env f xs = xs <#> (\x -> act env $ f x) # sequence_

type Options
  = { username :: String
    , email :: String
    }

signUpUser :: forall m. Monad m => Env m -> Options -> StateT CirclesState m Unit
signUpUser env opts = do
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

finalizeAccount :: forall m. Env m -> Options -> StateT CirclesState m Unit
finalizeAccount env = undefined
