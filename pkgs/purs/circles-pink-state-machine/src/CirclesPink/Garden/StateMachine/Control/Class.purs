module CirclesPink.Garden.StateMachine.Control.Class where

import Prelude

class Monad m <= MonadCircles m where
  sleep :: Int -> m Unit
