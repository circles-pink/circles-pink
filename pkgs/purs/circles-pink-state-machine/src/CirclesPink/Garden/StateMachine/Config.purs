module CirclesPink.Garden.StateMachine.Config
  ( CirclesConfig(..)
  ) where

import Prelude

import Data.Maybe (Maybe)

newtype CirclesConfig m = CirclesConfig
  { extractEmail :: Maybe (String -> m Unit)
  }
