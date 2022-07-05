module CirclesPink.Garden.StateMachine.Control.Class where

import Prelude

import Data.Int (toNumber)
import Effect.Aff (Aff, Milliseconds(..), delay)

class MonadSleep m <= MonadCircles m

class Monad m <= MonadSleep m where
  sleep :: Int -> m Unit

instance monadSleepAff :: MonadSleep Aff where
  sleep i = delay $ Milliseconds $ toNumber i

instance monadCircles :: MonadCircles Aff

