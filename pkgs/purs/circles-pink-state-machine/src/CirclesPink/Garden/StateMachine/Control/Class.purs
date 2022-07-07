module CirclesPink.Garden.StateMachine.Control.Class where

import Prelude

import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import Control.Monad.Reader (class MonadAsk)

class
  ( Monad m
  , MonadAsk (CirclesConfig m) m
  ) <=
  MonadCircles m where
  sleep :: Int -> m Unit
  throwException :: String -> m Unit
