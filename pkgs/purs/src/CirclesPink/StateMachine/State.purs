module CirclesPink.StateMachine.State where

import CirlesPink.StateMachine.Error (CirclesError)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData, _notAsked)
import Type.Proxy (Proxy(..))

-- import Wallet.PrivateKey (Mnemonic, PrivateKey)
type UserData
  = { username :: String
    , usernameApiResult :: RemoteData (Variant (CirclesError ())) { isValid :: Boolean }
    , email :: String
    , emailApiResult :: RemoteData (Variant (CirclesError ())) { isValid :: Boolean }
    , terms :: Boolean
    , privacy :: Boolean
    -- , privateKey :: PrivateKey
    }

type CirclesState
  = Variant
      ( infoGeneral :: UserData
      , askUsername :: UserData
      , askEmail :: UserData
      , infoSecurity :: UserData
      )

init :: CirclesState
init =
  _infoGeneral
    { username: ""
    , usernameApiResult: _notAsked
    , email: ""
    , emailApiResult: _notAsked
    , terms: false
    , privacy: false
    -- , privateKey: ""
    }

_infoGeneral :: forall a v. a -> Variant ( infoGeneral :: a | v )
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant ( askUsername :: a | v )
_askUsername = inj (Proxy :: _ "askUsername")

_askEmail :: forall a v. a -> Variant ( askEmail :: a | v )
_askEmail = inj (Proxy :: _ "askEmail")

_infoSecurity :: forall a v. a -> Variant ( infoSecurity :: a | v )
_infoSecurity = inj (Proxy :: _ "infoSecurity")
