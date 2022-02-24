module CirclesPink.StateMachine.Action where

import Prelude
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type CirclesAction
  = Variant
      ( infoGeneral ::
          Variant
            ( next :: Unit
            )
      , askUsername ::
          Variant
            ( prev :: Unit
            , setUsername :: String
            , next :: Unit
            )
      , askEmail ::
          Variant
            ( prev :: Unit
            , setEmail :: String
            )
      )

----
_infoGeneral :: forall a v. a -> Variant ( infoGeneral :: a | v )
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant ( askUsername :: a | v )
_askUsername = inj (Proxy :: _ "askUsername")

_askEmail :: forall a v. a -> Variant ( askEmail :: a | v )
_askEmail = inj (Proxy :: _ "askEmail")

----
_next :: forall a v. a -> Variant ( next :: a | v )
_next = inj (Proxy :: _ "next")

_prev :: forall a v. a -> Variant ( prev :: a | v )
_prev = inj (Proxy :: _ "prev")

_setUsername :: forall a v. a -> Variant ( setUsername :: a | v )
_setUsername = inj (Proxy :: _ "setUsername")

_setEmail :: forall a v. a -> Variant ( setEmail :: a | v )
_setEmail = inj (Proxy :: _ "setEmail")
