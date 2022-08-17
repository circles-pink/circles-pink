module CirclesPink.Garden.StateMachine.TrackingEvent
  ( TrackedCirclesAction(..)
  , TrackedCirclesState(..)
  , TrackedRemoteData(..)
  , TrackingEvent(..)
  , _askEmail
  , _askUsername
  , _dashboard
  , _infoSecurity
  , _landing
  , _login
  , _magicWords
  , _next
  , _prev
  , _submit
  , _trusts
  , class GenEncodeJsonVariant
  , convertAction
  , convertState
  , encodeJsonTrackingEvent
  , encodeJsonVariant
  , fromAction
  , fromStateUpdate
  , genEncodeJsonVariant
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, case_, default, inj, onMatch, prj)
import Partial.Unsafe (unsafeCrashWith)
import Prim.RowList (class RowToList, Cons, Nil)
import RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))
import Type.Row (class Cons)

--------------------------------------------------------------------------------
data TrackingEvent
  = NewState TrackedCirclesState
  | Action TrackedCirclesAction

derive instance genericTrackingEvent :: Generic TrackingEvent _

instance encodeJsonTrackingEventInst :: EncodeJson TrackingEvent where
  encodeJson = genericEncodeJson

--------------------------------------------------------------------------------

newtype TrackedCirclesState = TrackedCirclesState
  ( Variant
      ( askUsername :: Unit
      , askEmail :: Unit
      , infoSecurity :: Unit
      , magicWords :: Unit
      , submit :: { submitResult :: TrackedRemoteData }
      , dashboard :: Unit
      , login :: Unit
      , trusts ::
          { deploySafeResult :: TrackedRemoteData
          , deployTokenResult :: TrackedRemoteData
          }
      , debug :: Unit
      , landing :: Unit
      )
  )

derive instance newtypeTrackedCirclesState :: Newtype TrackedCirclesState _

derive instance eqTrackedCirclesState :: Eq TrackedCirclesState

instance encodeJsonTrackedCirclesState :: EncodeJson TrackedCirclesState where
  encodeJson (TrackedCirclesState x) = encodeJsonVariant x

--------------------------------------------------------------------------------

data TrackedRemoteData = NotAsked | Loading | Failure | Success

derive instance genericTrackedRemoteData :: Generic TrackedRemoteData _
derive instance eqTrackedRemoteData :: Eq TrackedRemoteData

instance encodeJsonTrackedRemoteData :: EncodeJson TrackedRemoteData where
  encodeJson = genericEncodeJson

convertRemoteData :: forall n l e a. RemoteData n l e a -> TrackedRemoteData
convertRemoteData (RemoteData v) =
  ( case_ # onMatch
      { notAsked: \_ -> NotAsked
      , loading: \_ -> Loading
      , failure: \_ -> Failure
      , success: \_ -> Success
      }
  ) v

--------------------------------------------------------------------------------

newtype TrackedCirclesAction = TrackedCirclesAction
  ( Variant
      ( askUsername ::
          Variant
            ( prev :: Unit
            , next :: Unit
            )
      , askEmail ::
          Variant
            ( prev :: Unit
            , next :: Unit
            )
      , infoSecurity ::
          Variant
            ( prev :: Unit
            , next :: Unit
            )
      , magicWords ::
          Variant
            ( prev :: Unit
            , next :: Unit
            )
      , submit ::
          Variant
            ( prev :: Unit
            , submit :: Unit
            )
      , dashboard ::
          Variant
            ( logout :: Unit
            )
      , login ::
          Variant
            ( login :: Unit
            , signUp :: Unit
            )
      , trusts ::
          Variant
            ( finalizeRegisterUser :: Unit
            )
      , landing ::
          Variant
            ( signUp :: Unit
            , signIn :: Unit
            )
      )
  )

derive instance newtypeTrackedCirclesAction :: Newtype TrackedCirclesAction _

derive instance eqTrackedCirclesAction :: Eq TrackedCirclesAction

instance encodeJsonTrackedCirclesAction :: EncodeJson TrackedCirclesAction where
  encodeJson (TrackedCirclesAction x) = encodeJsonVariant x

--------------------------------------------------------------------------------

encodeJsonTrackingEvent :: TrackingEvent -> Json
encodeJsonTrackingEvent = encodeJson

fromAction :: CirclesState -> CirclesAction -> Maybe TrackingEvent
fromAction _ ac = Action <$> convertAction ac

fromStateUpdate :: { prev :: CirclesState, next :: CirclesState } -> Maybe TrackingEvent
fromStateUpdate { prev, next } = if prev' /= next' then NewState <$> next' else Nothing
  where
  prev' = convertState prev
  next' = convertState next

convertState :: CirclesState -> Maybe TrackedCirclesState
convertState = case_ # onMatch
  { askUsername: \_ -> Just $ wrap $ inj _askUsername unit
  , askEmail: \_ -> Just $ wrap $ inj _askEmail unit
  , infoSecurity: \_ -> Just $ wrap $ inj _infoSecurity unit
  , magicWords: \_ -> Just $ wrap $ inj _magicWords unit
  , submit: \s -> Just $ wrap $ inj _submit
      { submitResult: convertRemoteData s.submitResult
      }
  , dashboard: \_ -> Just $ wrap $ inj _dashboard unit
  , login: \_ -> Just $ wrap $ inj _login unit
  , trusts: \s -> Just $ wrap $ inj _trusts
      { deploySafeResult: convertRemoteData s.deploySafeResult
      , deployTokenResult: convertRemoteData s.deployTokenResult
      }
  , landing: \_ -> Just $ wrap $ inj _landing unit
  , debug: \_ -> Nothing
  }

convertAction :: CirclesAction -> Maybe TrackedCirclesAction
convertAction = default Nothing # onMatch
  { askUsername:
      default Nothing # onMatch
        { next: \_ -> Just $ wrap $ inj _askUsername $ inj _next unit
        }

  , askEmail:
      default Nothing # onMatch
        { next: \_ -> Just $ wrap $ inj _askEmail $ inj _next unit
        , prev: \_ -> Just $ wrap $ inj _askEmail $ inj _prev unit
        }

  , infoSecurity:
      default Nothing # onMatch
        { next: \_ -> Just $ wrap $ inj _infoSecurity $ inj _next unit
        , prev: \_ -> Just $ wrap $ inj _infoSecurity $ inj _prev unit
        }

  , magicWords:
      default Nothing # onMatch
        { next: \_ -> Just $ wrap $ inj _magicWords $ inj _next unit
        , prev: \_ -> Just $ wrap $ inj _magicWords $ inj _prev unit
        }

  , submit:
      default Nothing # onMatch
        { submit: \_ -> Just $ wrap $ inj _submit $ inj _submit unit
        , prev: \_ -> Just $ wrap $ inj _submit $ inj _prev unit
        }

  , dashboard:
      default Nothing # onMatch
        { logout: \_ -> Just $ wrap $ inj _dashboard $ inj _logout unit
        }

  , login:
      default Nothing # onMatch
        { login: \_ -> Just $ wrap $ inj _login $ inj _login unit
        , signUp: \_ -> Just $ wrap $ inj _login $ inj _signUp unit
        }

  , trusts:
      default Nothing # onMatch
        { finalizeRegisterUser: \_ -> Just $ wrap $ inj _trusts $ inj _finalizeRegisterUser unit

        }

  , debug: \_ ->
      Nothing

  , landing:
      default Nothing # onMatch
        { signUp: \_ -> Just $ wrap $ inj _landing $ inj _signUp unit
        , signIn: \_ -> Just $ wrap $ inj _landing $ inj _signIn unit
        }

  }

--------------------------------------------------------------------------------
-- Proxies
--------------------------------------------------------------------------------

_askUsername = Proxy :: Proxy "askUsername"
_askEmail = Proxy :: Proxy "askEmail"
_infoSecurity = Proxy :: Proxy "infoSecurity"
_magicWords = Proxy :: Proxy "magicWords"
_submit = Proxy :: Proxy "submit"
_dashboard = Proxy :: Proxy "dashboard"
_login = Proxy :: Proxy "login"
_trusts = Proxy :: Proxy "trusts"
_landing = Proxy :: Proxy "landing"
_next = Proxy :: Proxy "next"

_prev = Proxy :: Proxy "prev"

_logout = Proxy :: Proxy "logout"

_signUp = Proxy :: Proxy "signUp"

_finalizeRegisterUser = Proxy :: Proxy "finalizeRegisterUser"

_signIn = Proxy :: Proxy "signIn"

--------------------------------------------------------------------------------

encodeJsonVariant :: forall r rl. RowToList r rl => GenEncodeJsonVariant r rl => Variant r -> Json
encodeJsonVariant v = genEncodeJsonVariant v (Proxy :: _ rl)

class GenEncodeJsonVariant :: forall k. Row Type -> k -> Constraint
class GenEncodeJsonVariant r rl where
  genEncodeJsonVariant :: Variant r -> Proxy rl -> Json

instance genEncodeJsonVariantNil :: GenEncodeJsonVariant v Nil where
  genEncodeJsonVariant _ _ = unsafeCrashWith "should not be possible"

instance genEncodeJsonVariantRec' ::
  ( IsSymbol s
  , Cons s (Variant v') v_ v
  , GenEncodeJsonVariant v rl
  , GenEncodeJsonVariant v' v'rl
  , RowToList v' v'rl
  ) =>
  GenEncodeJsonVariant v (Cons s (Variant v') rl) where
  genEncodeJsonVariant v _ = case prj (Proxy :: _ s) v of
    Just val -> encodeJson { tag: reflectSymbol (Proxy :: _ s), value: genEncodeJsonVariant val (Proxy :: _ v'rl) }
    Nothing -> genEncodeJsonVariant v (Proxy :: _ rl)

else instance genEncodeJsonVariantRec ::
  ( IsSymbol s
  , Cons s t v_ v
  , GenEncodeJsonVariant v rl
  , EncodeJson t
  ) =>
  GenEncodeJsonVariant v (Cons s t rl) where
  genEncodeJsonVariant v _ = case prj (Proxy :: _ s) v of
    Just val -> encodeJson { tag: reflectSymbol (Proxy :: _ s), value: encodeJson val }
    Nothing -> genEncodeJsonVariant v (Proxy :: _ rl)
