module CirclesPink.Garden.StateMachine.TrackingEvent where

import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

data TrackingEvent = TrackingEvent

derive instance genericTrackingEvent :: Generic TrackingEvent _

instance encodeJsonTrackingEventInst :: EncodeJson TrackingEvent where
  encodeJson = genericEncodeJson

encodeJsonTrackingEvent :: TrackingEvent -> Json
encodeJsonTrackingEvent = encodeJson

fromAction ::  CirclesState -> CirclesAction -> Maybe TrackingEvent
fromAction _ _ = Nothing

fromStateUpdate :: {prev:: CirclesState, next :: CirclesState} -> Maybe TrackingEvent
fromStateUpdate _ = Nothing