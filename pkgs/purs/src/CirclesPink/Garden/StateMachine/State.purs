module CirclesPink.Garden.StateMachine.State where

import Prelude
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData, _notAsked)
import Type.Proxy (Proxy(..))
import Wallet.PrivateKey (PrivateKey)
import Wallet.PrivateKey as P

type UsernameApiResult
  = RemoteData CirclesError { isValid :: Boolean }

type EmailApiResult
  = RemoteData CirclesError { isValid :: Boolean }

type UserData
  = { direction :: D.Direction
    , username :: String
    , usernameApiResult :: UsernameApiResult
    , email :: String
    , emailApiResult :: EmailApiResult
    , terms :: Boolean
    , privacy :: Boolean
    , privateKey :: PrivateKey
    }

type CirclesState
  = Variant
      ( infoGeneral :: UserData
      , askUsername :: UserData
      , askEmail :: UserData
      , infoSecurity :: UserData
      , magicWords :: UserData
      , submit :: UserData
      , dashboard :: UserData
      )

init :: CirclesState
init =
  _infoGeneral
    { direction: D._forwards
    , username: ""
    , usernameApiResult: _notAsked
    , email: ""
    , emailApiResult: _notAsked
    , terms: false
    , privacy: false
    , privateKey: P.zeroKey
    }

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
