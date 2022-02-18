module CirclesPink.StateMachine.State
  ( CirclesState
  , UserData
  , UsernameCheckResult
  , _askUsername
  , _infoGeneral
  , init
  ) where

import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type UsernameCheckResult
  = { username :: String
    , isValid :: Boolean
    }

type UserData
  = { username :: String
    , usernameCheckResult :: UsernameCheckResult
    }

type CirclesState
  = Variant
      ( infoGeneral :: UserData
      , askUsername :: UserData
      )

init :: CirclesState
init =
  _infoGeneral
    { username: ""
    , usernameCheckResult:
        { username: ""
        , isValid: false
        }
    }

_infoGeneral :: forall a v. a -> Variant ( infoGeneral :: a | v )
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant ( askUsername :: a | v )
_askUsername = inj (Proxy :: _ "askUsername")
