module CirclesPink.Garden.StateMachine.Action where

import Prelude
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type CirclesAction
  = Variant
      ( landing ::
          Variant
            ( signIn :: Unit
            , signUp :: Unit
            )
      , infoGeneral ::
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
            , setMagicWords :: String
            )
      , trusts ::
          Variant
            ( continue :: Unit
            )
      , debug ::
          Variant
            ( coreToWindow :: Unit
            )
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

_landing :: forall a v. a -> Variant ( landing :: a | v )
_landing = inj (Proxy :: _ "landing")

_trusts :: forall a v. a -> Variant ( trusts :: a | v )
_trusts = inj (Proxy :: _ "trusts")

_login :: forall a v. a -> Variant ( login :: a | v )
_login = inj (Proxy :: _ "login")

_debug :: forall a v. a -> Variant ( debug :: a | v )
_debug = inj (Proxy :: _ "debug")

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

_newPrivKey :: forall a v. a -> Variant ( newPrivKey :: a | v )
_newPrivKey = inj (Proxy :: _ "newPrivKey")

_signIn :: forall a v. a -> Variant ( signIn :: a | v )
_signIn = inj (Proxy :: _ "signIn")

_signUp :: forall a v. a -> Variant ( signUp :: a | v )
_signUp = inj (Proxy :: _ "signUp")

_setMagicWords :: forall a v. a -> Variant ( setMagicWords :: a | v )
_setMagicWords = inj (Proxy :: _ "setMagicWords")

_continue :: forall a v. a -> Variant ( continue :: a | v )
_continue = inj (Proxy :: _ "continue")

_coreToWindow :: forall a v. a -> Variant ( coreToWindow :: a | v )
_coreToWindow = inj (Proxy :: _ "coreToWindow")
