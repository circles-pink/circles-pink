module CirclesPink.Garden.ApiSimulation where


-- import Control.Monad.Gen (class MonadGen, chooseInt)
-- import Control.Monad.State (class MonadState, get)
-- import Data.Map (Map)
-- import Data.Typelevel.Undefined (undefined)
-- import CirclesPink.Data.Address (Address)

-- type SimulationState =
--   { users ::
--       Map Address
--         { username :: String
--         , email :: String
--         }
--   , newUser ::
--       {
--       }
--   }

-- class (MonadState SimulationState m, MonadGen m) <= SimC m

-- init :: forall m. SimC m => m Unit
-- init = do
--   s <- get
--   r <- chooseInt 1 2
--   pure unit