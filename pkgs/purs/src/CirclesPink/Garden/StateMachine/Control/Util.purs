module CirclesPink.Garden.StateMachine.Control.Util where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either)
import Data.Variant (Variant)
import Prim.Row (class Nub)
import Unsafe.Coerce (unsafeCoerce)

run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

run' :: forall t m e e' a. MonadTrans t => Nub e e' => Monad m => ExceptV e m a -> t m (Either (Variant e') a)
run' = lift <<< runExceptT'
  where
  runExceptT' :: ExceptV e m a -> m (Either (Variant e') a)
  runExceptT' = unsafeCoerce <<< runExceptT

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v
  = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit
