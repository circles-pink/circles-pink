module Log.Class where

import Prelude

import Control.Monad.State (StateT, lift)
import Data.Identity (Identity)
import Effect.Aff (Aff)
import Effect.Class.Console as A

class MonadLog :: (Type -> Type) -> Constraint
class Monad m <= MonadLog m where
  log :: String -> m Unit

instance _MonadLogIdentity :: MonadLog Identity where
  log _ = pure unit

instance _MonadLogAff :: MonadLog Aff where
  log = A.log

instance _MonadLogStateT :: MonadLog m => MonadLog (StateT s m) where
  log = lift <<< log

