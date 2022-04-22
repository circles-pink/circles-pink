module CirclesPink.Garden.StateMachine.State
  ( CirclesState
  , DashboardState
  , DebugState
  , EmailApiResult
  , LandingState
  , LoginState
  , TrustState
  , UserData
  , UsernameApiResult
  , _askEmail
  , _askUsername
  , _dashboard
  , _debug
  , _infoGeneral
  , _infoSecurity
  , _landing
  , _login
  , _magicWords
  , _submit
  , _trusts
  , init
  , initDebug
  , initLanding
  , initLogin
  ) where

import Prelude
import CirclesCore (ApiError, NativeError, TrustNode, SafeStatus)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import RemoteData (RemoteData, _notAsked)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey)
import Wallet.PrivateKey as P

type UsernameApiResult
  = RemoteData CirclesError { isValid :: Boolean }

type EmailApiResult
  = RemoteData CirclesError { isValid :: Boolean }

type LandingState
  = {}

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

type LoginState
  = { magicWords :: String
    , error ::
        Maybe
          ( Variant
              ( errApi ∷ ApiError
              , errNative ∷ NativeError
              , errUserNotFound ∷ UserNotFoundError
              , errInvalidUrl :: String
              )
          )
    }

type DashboardState
  = { user :: CC.User
    , privKey :: PrivateKey
    , trusts :: Array TrustNode
    , error ::
        Maybe
          ( Variant
              ( errService ∷ Unit
              , errNative ∷ NativeError
              , errInvalidUrl :: String
              )
          )
    }

type TrustState
  = { user :: CC.User
    , privKey :: PrivateKey
    , trusts :: Array TrustNode
    , safeStatus :: SafeStatus
    , isReady :: Boolean
    , error ::
        Maybe
          ( Variant
              ( errInvalidUrl :: String
              , errNative :: NativeError
              , errService :: Unit
              )
          )
    }

type CirclesState
  = Variant
      ( landing :: LandingState
      , infoGeneral :: UserData
      , askUsername :: UserData
      , askEmail :: UserData
      , infoSecurity :: UserData
      , magicWords :: UserData
      , submit :: UserData
      , dashboard :: DashboardState
      , login :: LoginState
      , trusts :: TrustState
      , debug :: DebugState
      )

type DebugState
  = { magicWords :: String
    }

-- init :: CirclesState
init ∷ forall v. Variant ( infoGeneral :: UserData | v )
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

initLanding :: forall v. Variant ( landing :: LandingState | v )
initLanding =
  _landing
    {}

initLogin :: forall v. Variant ( login :: LoginState | v )
initLogin =
  _login
    { magicWords: ""
    , error: Nothing
    }

initDebug :: forall v. Variant ( debug :: DebugState | v )
initDebug =
  _debug
    { magicWords: "hockey middle idea enable forget case mountain sugar chronic income crouch critic venue giant tell marble rose scene prefer shoe cheap run print pigeon"
    }

_landing :: forall a v. a -> Variant ( landing :: a | v )
_landing = inj (Proxy :: _ "landing")

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

_login :: forall a v. a -> Variant ( login :: a | v )
_login = inj (Proxy :: _ "login")

_trusts :: forall a v. a -> Variant ( trusts :: a | v )
_trusts = inj (Proxy :: _ "trusts")

_debug :: forall a v. a -> Variant ( debug :: a | v )
_debug = inj (Proxy :: _ "debug")
