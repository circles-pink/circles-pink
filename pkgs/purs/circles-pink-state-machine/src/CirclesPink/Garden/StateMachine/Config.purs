module CirclesPink.Garden.StateMachine.Config
  ( CirclesConfig(..)
  , mapCirclesConfig
  ) where

import Prelude

import Data.Maybe (Maybe)

newtype CirclesConfig m = CirclesConfig
  { extractEmail :: Maybe (String -> m Unit)
  }

mapCirclesConfig :: forall (m :: Type -> Type) n a. (m Unit -> n Unit) -> CirclesConfig m -> CirclesConfig n
mapCirclesConfig f (CirclesConfig x) = CirclesConfig
  { extractEmail: map (map f) x.extractEmail
  }