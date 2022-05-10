module CirclesPink.Garden.StateMachine.Stories
  ( ScriptT
  , SignUpUserOpts
  , finalizeAccount
  , runScripT
  , signUpUser
  ) where

import Prelude
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState, init)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State (StateT, get, runStateT)
import Data.Either (Either)
import Data.Traversable (sequence_)
import Data.Tuple.Nested (type (/\))
import Data.Variant (default, onMatch)
import Stadium.Control (toStateT)
import Undefined (undefined)
import Wallet.PrivateKey (PrivateKey)

type ScriptT m a
  = ExceptT String (StateT CirclesState m) a

runScripT :: forall m a. ScriptT m a -> m (Either String a /\ CirclesState)
runScripT = flip runStateT init <<< runExceptT

--------------------------------------------------------------------------------
act :: forall m. Monad m => Env m -> CirclesAction -> StateT CirclesState m Unit
act env ac = do
  --log ("ACTION: " <> show ac)
  --_ <- get
  toStateT (circlesControl env) ac

--log ("STATE: " <> show st)
--log ""
act' :: forall m a. Monad m => Env m -> (a -> CirclesAction) -> Array a -> StateT CirclesState m Unit
act' env f xs = xs <#> (\x -> act env $ f x) # sequence_

--------------------------------------------------------------------------------
type SignUpUserOpts
  = { username :: String
    , email :: String
    }

signUpUser :: forall m. Monad m => Env m -> SignUpUserOpts -> ExceptT String (StateT CirclesState m) PrivateKey
signUpUser env opts =
  ExceptT do
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
    get <#> (default (throwError "nooo!") # onMatch { "trusts": _.privKey >>> pure })

--------------------------------------------------------------------------------
finalizeAccount :: forall m. Monad m => Env m -> ExceptT String (StateT CirclesState m) Unit
finalizeAccount env = undefined
