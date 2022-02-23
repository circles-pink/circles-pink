module RemoteData where

import Prelude
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type RemoteData e a
  = Variant
      ( notAsked :: Unit
      , loading :: Unit
      , failure :: e
      , success :: a
      )

_notAsked :: forall e a. RemoteData e a
_notAsked = inj (Proxy :: _ "notAsked") unit

_loading :: forall e a. RemoteData e a
_loading = inj (Proxy :: _ "loading") unit

_failure :: forall e a. e -> RemoteData e a
_failure = inj (Proxy :: _ "failure")

_success :: forall e a. a -> RemoteData e a
_success = inj (Proxy :: _ "success")
