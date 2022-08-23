module CirclesPink.Garden.StateMachine.Config
  ( CirclesConfig(..)
  , mapCirclesConfig
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.TrackingEvent (TrackingEvent)
import CirclesPink.Garden.StateMachine.TrackingResumee (Resumee)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype CirclesConfig m = CirclesConfig
  { extractEmail :: Either String (String -> m Unit)
  , onTrackingEvent :: Maybe (TrackingEvent -> m Unit)
  , onTrackingResumee :: Maybe ((Resumee -> Resumee) -> m Unit)
  }

derive instance newtypeCirclesConfig :: Newtype (CirclesConfig m) _

mapCirclesConfig :: forall (m :: Type -> Type) n. (m Unit -> n Unit) -> CirclesConfig m -> CirclesConfig n
mapCirclesConfig f (CirclesConfig x) = CirclesConfig
  { extractEmail: map (map f) x.extractEmail
  , onTrackingEvent: map (map f) x.onTrackingEvent
  , onTrackingResumee: map (map f) x.onTrackingResumee
  }