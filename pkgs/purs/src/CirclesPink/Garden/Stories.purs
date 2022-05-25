module CirclesPink.Garden.StateMachine.Stories
  ( Err
  , ScriptT
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
import Control.Monad.Except (class MonadTrans, ExceptT(..), runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.State (StateT, get, runStateT)
import Data.Either (Either)
import Data.Traversable (sequence_)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, default, inj, onMatch)
import Data.Variant.Extra (getLabel)
import Debug (spy)
import Effect.Class (class MonadEffect)
import Log.Class (class MonadLog, log)
import Stadium.Control (toStateT)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey as CC

type ScriptT e m a = ExceptV e (StateT CirclesState m) a

runScripT :: forall e m a. ScriptT e m a -> m (Either (Variant e) a /\ CirclesState)
runScripT = flip runStateT init <<< runExceptT

--------------------------------------------------------------------------------
act :: forall m. MonadLog m => Env (StateT CirclesState m) -> CirclesAction -> StateT CirclesState m Unit
act env =
  let
    z = circlesControl env
    ctl = toStateT (circlesControl env)
  in
    \ac -> do
      log ("ACTION: " <> show ac)
      ctl ac
      st <- get
      log ("STATE: " <> getLabel st)
      let
        x = spy "st" st
      log ""

-- act' :: forall m a. MonadLog m => Env m -> (a -> CirclesAction) -> Array a -> StateT CirclesState m Unit
-- act' env f xs = xs <#> (\x -> act env $ f x) # sequence_

--------------------------------------------------------------------------------
type Err r = (err :: String | r)

err :: forall r. String -> Variant (err :: String | r)
err = inj (Proxy :: _ "err")

--------------------------------------------------------------------------------
type SignUpUserOpts =
  { username :: String
  , email :: String
  }

type SignUpUser =
  { privateKey :: CC.PrivateKey
  , safeAddress :: CC.Address
  }

signUpUser :: forall t m r. MonadLog m => Env (StateT CirclesState m) -> SignUpUserOpts -> ScriptT (Err + r) m SignUpUser
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
    get
      <#>
        ( default (throwError $ err "Cannot sign up user.")
            # onMatch
                { trusts: \x -> pure
                      { privateKey: x.privKey
                      , safeAddress: x.user.safeAddress
                      }
                }
        )

--------------------------------------------------------------------------------
finalizeAccount :: forall m r. MonadLog m => Env (StateT CirclesState m) -> ScriptT (Err + r) m Unit
finalizeAccount env =
  ExceptT do
    act env $ A._trusts $ A._finalizeRegisterUser unit
    get
      <#>
        ( default (throwError $ err "Cannot finalize register user.")
            # onMatch
                { dashboard: \_ -> pure unit
                }
        )
