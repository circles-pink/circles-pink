module CirclesPink.Garden.StateMachine.Control.Class where

import Prelude

import Data.Int (toNumber)
import Effect.Aff (Aff, Milliseconds(..), delay)


class Monad m <= MonadCircles m where
  sleep :: Int -> m Unit
  --getSafeAddress :: forall r. String -> ExceptV (ErrGetSafeAddress r) m { isValid :: Boolean }

instance monadCirclesAff :: MonadCircles Aff where
  sleep i = delay $ Milliseconds $ toNumber i
  --getSafeAddress = todo