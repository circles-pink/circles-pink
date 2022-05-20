module CirclesPink.Garden.StateMachine.Control.Common where

import Prelude
import CirclesCore (SafeStatus, TrustNode, User)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Prim.Row (class Nub)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Wallet.PrivateKey (PrivateKey)

run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

run' :: forall t m e e' a. MonadTrans t => Nub e e' => Monad m => ExceptV e m a -> t m (Either (Variant e') a)
run' = lift <<< runExceptT'

runExceptT' :: forall m e e' a. ExceptV e m a -> m (Either (Variant e') a)
runExceptT' = unsafeCoerce <<< runExceptT

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit

type ActionHandler' m a s v = ((s -> Variant v) -> m Unit) -> s -> a -> m Unit

--------------------------------------------------------------------------------
type ErrReadyForDeployment r = Env.ErrIsTrusted + Env.ErrIsFunded + r

readyForDeployment :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrReadyForDeployment r) m Boolean
readyForDeployment { isTrusted, isFunded } privKey = do
  isTrusted' <- isTrusted privKey <#> (unwrap >>> _.isTrusted)
  isFunded' <- isFunded privKey
  pure (isTrusted' || isFunded')

--------------------------------------------------------------------------------
type TaskReturn =
  { user :: User
  , isTrusted :: Boolean
  , trusts :: Array TrustNode
  , safeStatus :: SafeStatus
  , isReady :: Boolean
  }

loginTask :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrLoginTask + r) m TaskReturn
loginTask env privKey = do
  user <- env.userResolve privKey
  safeStatus <- env.getSafeStatus privKey
  isTrusted <- env.isTrusted privKey <#> (unwrap >>> _.isTrusted)
  trusts <- if isTrusted then pure [] else env.trustGetNetwork privKey
  isReady' <- readyForDeployment env privKey
  pure { user, isTrusted, trusts, safeStatus, isReady: isReady' }

--------------------------------------------------------------------------------
