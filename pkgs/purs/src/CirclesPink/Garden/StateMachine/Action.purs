module CirclesPink.Garden.StateMachine.Action where

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
            , setTerms :: Unit
            , setPrivacy :: Unit
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
      , dashboard :: Variant ()
      )

----
_infoGeneral :: forall a v. a -> Variant ( infoGeneral :: a | v )
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant ( askUsername :: a | v )
_askUsername = inj (Proxy :: _ "askUsername")

_askEmail :: forall a v. a -> Variant ( askEmail :: a | v )
_askEmail = inj (Proxy :: _ "askEmail")

_infoSecurity :: forall a v. a -> Variant ( infoSecurity :: a | v )
_infoSecurity = inj (Proxy :: _ "infoSecurity")

_magicWords :: forall a v. a -> Variant ( magicWords :: a | v )
_magicWords = inj (Proxy :: _ "magicWords")

_submit :: forall a v. a -> Variant ( submit :: a | v )
_submit = inj (Proxy :: _ "submit")

_dashboard :: forall a v. a -> Variant ( dashboard :: a | v )
_dashboard = inj (Proxy :: _ "dashboard")

----
_next :: forall a v. a -> Variant ( next :: a | v )
_next = inj (Proxy :: _ "next")

_prev :: forall a v. a -> Variant ( prev :: a | v )
_prev = inj (Proxy :: _ "prev")

_setUsername :: forall a v. a -> Variant ( setUsername :: a | v )
_setUsername = inj (Proxy :: _ "setUsername")

_setEmail :: forall a v. a -> Variant ( setEmail :: a | v )
_setEmail = inj (Proxy :: _ "setEmail")

_setTerms :: forall a v. a -> Variant ( setTerms :: a | v )
_setTerms = inj (Proxy :: _ "setTerms")

_setPrivacy :: forall a v. a -> Variant ( setPrivacy :: a | v )
_setPrivacy = inj (Proxy :: _ "setPrivacy")
