module CirclesPink.StateMachine.State
  ( CirclesState
  , UserData
  , _askUsername
  , _infoGeneral
  , init
  ) where

import Prelude
import Data.Variant (Variant, inj)
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))

type UserData
  = { username :: String
    , usernameApiResult :: RemoteData Unit Unit
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
    , usernameApiResult: NotAsked
    }

_infoGeneral :: forall a v. a -> Variant ( infoGeneral :: a | v )
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant ( askUsername :: a | v )
_askUsername = inj (Proxy :: _ "askUsername")
