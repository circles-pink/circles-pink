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
            )
      )

----
_infoGeneral :: forall a v. a -> Variant ( infoGeneral :: a | v )
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant ( askUsername :: a | v )
_askUsername = inj (Proxy :: _ "askUsername")

----
_next :: forall a v. a -> Variant ( next :: a | v )
_next = inj (Proxy :: _ "next")

_prev :: forall a v. a -> Variant ( prev :: a | v )
_prev = inj (Proxy :: _ "prev")

_setUsername :: forall a v. a -> Variant ( setUsername :: a | v )
_setUsername = inj (Proxy :: _ "setUsername")
