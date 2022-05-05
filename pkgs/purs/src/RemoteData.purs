module RemoteData where

import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type RemoteData n l e a
  = Variant
      ( notAsked :: n
      , loading :: l
      , failure :: e
      , success :: a
      )

_notAsked :: forall n l e a. n -> RemoteData n l e a
_notAsked = inj (Proxy :: _ "notAsked")

_loading :: forall n l e a. l -> RemoteData n l e a
_loading = inj (Proxy :: _ "loading")

_failure :: forall n l e a. e -> RemoteData n l e a
_failure = inj (Proxy :: _ "failure")

_success :: forall n l e a. a -> RemoteData n l e a
_success = inj (Proxy :: _ "success")
