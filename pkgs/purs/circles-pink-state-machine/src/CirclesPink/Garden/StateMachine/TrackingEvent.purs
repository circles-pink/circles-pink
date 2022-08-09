module CirclesPink.Garden.StateMachine.TrackingEvent
  ( TrackedCirclesAction(..)
  , TrackedCirclesState(..)
  , TrackingEvent(..)
  , _askEmail
  , _askUsername
  , _dashboard
  , _infoGeneral
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
  )
  where

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
      ( infoGeneral :: Unit
      , askUsername :: Unit
      , askEmail :: Unit
      , infoSecurity :: Unit
      , magicWords :: Unit
      , submit :: Unit
      , dashboard :: Unit
      , login :: Unit
      , trusts :: Unit
      , debug :: Unit
      , landing :: Unit
      )
  )

derive instance newtypeTrackedCirclesState :: Newtype TrackedCirclesState _

derive instance eqTrackedCirclesState :: Eq TrackedCirclesState

instance encodeJsonTrackedCirclesState :: EncodeJson TrackedCirclesState where
  encodeJson (TrackedCirclesState x) = encodeJsonVariant x

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

--------------------------------------------------------------------------------

newtype TrackedCirclesAction = TrackedCirclesAction
  ( Variant
      ( infoGeneral ::
          Variant
            ( next :: Unit
            )
      , askUsername ::
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
            , newPrivKey :: Unit
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
            ( getSafeStatus :: Unit
            , finalizeRegisterUser :: Unit
            )
      , landing ::
          Variant
            ( signUp :: Unit
            , signIn :: Unit
            , checkForSession :: Unit
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
  { infoGeneral: \_ -> Just $ wrap $ inj _infoGeneral unit
  , askUsername: \_ -> Just $ wrap $ inj _askUsername unit
  , askEmail: \_ -> Just $ wrap $ inj _askEmail unit
  , infoSecurity: \_ -> Just $ wrap $ inj _infoSecurity unit
  , magicWords: \_ -> Just $ wrap $ inj _magicWords unit
  , submit: \_ -> Just $ wrap $ inj _submit unit
  , dashboard: \_ -> Just $ wrap $ inj _dashboard unit
  , login: \_ -> Just $ wrap $ inj _login unit
  , trusts: \_ -> Just $ wrap $ inj _trusts unit
  , landing: \_ -> Just $ wrap $ inj _landing unit
  , debug: \_ -> Nothing
  }

convertAction :: CirclesAction -> Maybe TrackedCirclesAction
convertAction = default Nothing # onMatch
  { infoGeneral:
      case_ # onMatch
        { next: \_ -> Just $ wrap $ inj _infoGeneral $ inj _next unit
        }

  , askUsername:
      default Nothing # onMatch
        { next: \_ -> Just $ wrap $ inj _askUsername $ inj _next unit
        , prev: \_ -> Just $ wrap $ inj _askUsername $ inj _prev unit
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

  }

--------------------------------------------------------------------------------
-- Proxies
--------------------------------------------------------------------------------

_infoGeneral = Proxy :: Proxy "infoGeneral"
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
