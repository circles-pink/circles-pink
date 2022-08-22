module CirclesPink.Garden.StateMachine.TrackingResumee
  ( Instant(..)
  , Resumee
  , StepName(..)
  , decodeJsonResumee
  , encodeJsonResumee
  , fromAction
  , fromStateUpdate
  , init
  )
  where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Garden.StateMachine (CirclesAction, CirclesState)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.DateTime.Instant (instant)
import Data.DateTime.Instant as DT
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, wrap)
import Data.Newtype.Extra ((-#))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (case_, default, onMatch)

newtype Instant = Instant DT.Instant

derive instance Newtype Instant _

instance EncodeJson Instant where
  encodeJson = un Instant >>> DT.unInstant >>> un Milliseconds >>> encodeJson

instance DecodeJson Instant where
  decodeJson x = decodeJson x <#> Milliseconds >>= instant >>> note (TypeMismatch "Not an instant") <#> Instant

data StepName
  = Debug
  | Landing
  | Login
  | AskUserName
  | AskEmail
  | InfoSecurity
  | MagicWords
  | Submit
  | Trusts
  | Dashboard

derive instance Eq StepName
derive instance Ord StepName
derive instance Generic StepName _

instance DecodeJson StepName where
  decodeJson = genericDecodeJson

instance EncodeJson StepName where
  encodeJson = genericEncodeJson

type Resumee =
  { lastState :: StepName
  , safeAddress :: Maybe Address
  , triggeredSends :: Int
  , triggeredTrusts :: Int
  , triggeredUntrusts :: Int
  , lastLogin :: Instant
  }

encodeJsonResumee :: Resumee -> Json
encodeJsonResumee = encodeJson

decodeJsonResumee :: Json -> Either JsonDecodeError Resumee
decodeJsonResumee = decodeJson

init :: Resumee
init =
  { lastState: AskUserName
  , safeAddress: Nothing
  , triggeredSends: 0
  , triggeredTrusts: 0
  , triggeredUntrusts: 0
  , lastLogin: wrap bottom
  }

getStepName :: CirclesState -> StepName
getStepName = case_ # onMatch
  { askUsername: const AskUserName
  , askEmail: const AskEmail
  , infoSecurity: const InfoSecurity
  , magicWords: const MagicWords
  , submit: const Submit
  , dashboard: const Dashboard
  , login: const Login
  , trusts: const Trusts
  , landing: const Landing
  , debug: const Debug
  }

fromAction :: Instant -> CirclesAction -> Maybe (Resumee -> Resumee)
fromAction _ = default Nothing # onMatch
  { dashboard:
      default Nothing # onMatch
        { addTrustConnection: \_ -> Just incSends
        , removeTrustConnection: \_ -> Just incTrusts
        , transfer: \_ -> Just incUnTrusts
        }
  }
  where
  incSends r = r { triggeredSends = r.triggeredSends + 1 }
  incTrusts r = r { triggeredTrusts = r.triggeredTrusts + 1 }
  incUnTrusts r = r { triggeredUntrusts = r.triggeredUntrusts + 1 }

fromStateUpdate :: Instant -> { prev :: CirclesState, next :: CirclesState } -> Maybe (Resumee -> Resumee)
fromStateUpdate time { prev, next } = Nothing
  # update
      do
        comingFromLandingOrLogin
        safeAddress <- goingToDashboard
        pure (setLogin >>> setSafeAddress safeAddress)

  # update
      let
        nextStepName = getStepName next
        prevStepName = getStepName prev
      in
        if prevStepName < nextStepName then
          Just $ setLastState nextStepName
        else Nothing

  where
  update Nothing Nothing = Nothing
  update Nothing (Just cf) = Just cf
  update (Just pf) Nothing = Just (pf)
  update (Just pf) (Just cf) = Just (pf >>> cf)

  setLogin r = r { lastLogin = time }
  setLastState s r = r { lastState = s }
  setSafeAddress sa r = r { safeAddress = Just sa }

  comingFromLandingOrLogin = prev #
    ( default Nothing # onMatch
        { login: \_ -> Just unit
        , landing: \_ -> Just unit
        }
    )

  goingToDashboard = next #
    ( default Nothing # onMatch
        { dashboard: \{ user } -> Just $ user -# _.safeAddress
        }
    )
