module CirclesPink.Garden.StateMachine.Config
  ( CirclesConfig(..)
  , mapCirclesConfig
  ) where

import Prelude

import Data.Either (Either)
import Data.Newtype (class Newtype)

newtype CirclesConfig m = CirclesConfig
  { extractEmail :: Either String (String -> m Unit)
  }

derive instance newtypeCirclesConfig :: Newtype (CirclesConfig m) _

mapCirclesConfig :: forall (m :: Type -> Type) n. (m Unit -> n Unit) -> CirclesConfig m -> CirclesConfig n
mapCirclesConfig f (CirclesConfig x) = CirclesConfig
  { extractEmail: map (map f) x.extractEmail
  }