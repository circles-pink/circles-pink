module CirclesPink.Garden.StateMachine.Control.Class where

import Prelude

import Debug.Extra (todo)
import Effect.Aff (Aff)

class Monad m <= MonadCircles m where
  sleep :: Int -> m Unit

instance monadCirclesAff :: MonadCircles Aff where
  sleep = todo