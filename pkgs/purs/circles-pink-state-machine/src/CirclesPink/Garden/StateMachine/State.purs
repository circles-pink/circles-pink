module CirclesPink.Garden.StateMachine.State
  ( AskEmailState
  , AskUsernameState
  , CirclesState
  , DebugState
  , EmailApiResult
  , ErrLandingState
  , ErrLandingStateResolved
  , ErrSubmit
  , ErrSubmitResolved
  , InfoGeneralState
  , InfoSecurityState
  , LandingState
  , LandingStateCheckSessionResult
  , MagicWordsState
  , SubmitState
  , UserData
  , UserDataSubmitResult
  , UsernameApiResult
  , _askEmail
  , _askUsername
  , _debug
  , _infoGeneral
  , _infoSecurity
  , _landing
  , _magicWords
  , _submit
  , init
  , initDebug
  , initLanding
  , module Exp
  )
  where

import Prelude

import CirclesCore (ApiError, NativeError)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Garden.StateMachine.Control.Common (ErrLoginTask)
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError, RequestPath)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State.Dashboard (DashboardState)
import CirclesPink.Garden.StateMachine.State.Dashboard (DashboardState, ErrGetUsers, ErrTokenCheckUBIPayout, ErrTokenGetBalance, ErrTokenRequestUBIPayout, ErrTokenTransfer, ErrTrustAddConnection, ErrTrustGetTrusts, ErrUserSearch, GetUsersResult, InitDashboard, TokenCheckUBIPayoutResult, TokenGetBalanceResult, TokenRequestUBIPayoutResult, TokenTransferResult, TrustAddResult, TrustGetTrusts, _dashboard, initDashboard) as Exp
import CirclesPink.Garden.StateMachine.State.Login (LoginState)
import CirclesPink.Garden.StateMachine.State.Login (LoginState, ErrLoginState, initLogin, _login) as Exp
import CirclesPink.Garden.StateMachine.State.Trusts (TrustState)
import CirclesPink.Garden.StateMachine.State.Trusts (TrustState, ErrTrustState, TrustsDeploySafeResult, TrustsDeployTokenResult, _trusts) as Exp
import Data.Argonaut (JsonDecodeError)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import RemoteData (RemoteData, _notAsked)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type UsernameApiResult = RemoteData Unit Unit CirclesError { isValid :: Boolean }

type EmailApiResult = RemoteData Unit Unit CirclesError { isValid :: Boolean }

--------------------------------------------------------------------------------
-- UserData
--------------------------------------------------------------------------------
type ErrSubmit = Env.ErrGetSafeAddress
  + Env.ErrPrepareSafeDeploy
  + Env.ErrUserRegister
  + Env.ErrUserResolve
  + Env.ErrTrustGetNetwork
  + Env.ErrGetSafeStatus
  + Env.ErrSaveSession
  + ()

type ErrSubmitResolved = Variant
  ( errApi :: ApiError
  , errInvalidUrl :: String
  , errNative :: NativeError
  , errService :: Unit
  , errUserNotFound :: UserNotFoundError
  , errSaveSession :: Unit
  )

type UserDataSubmitResult = RemoteData Unit Unit ErrSubmitResolved Unit

type UserData =
  { direction :: D.Direction
  , username :: String
  , usernameApiResult :: UsernameApiResult
  , email :: String
  , emailApiResult :: EmailApiResult
  , terms :: Boolean
  , privacy :: Boolean
  , privateKey :: Maybe PrivateKey
  , submitResult :: UserDataSubmitResult
  }

type InfoGeneralState = UserData

type AskUsernameState = UserData

type AskEmailState = UserData

type InfoSecurityState = UserData

type MagicWordsState = UserData

type SubmitState = UserData

--------------------------------------------------------------------------------
type CirclesState = Variant
  ( infoGeneral :: UserData
  , askUsername :: UserData
  , askEmail :: UserData
  , infoSecurity :: UserData
  , magicWords :: UserData
  , submit :: UserData
  , dashboard :: DashboardState
  , login :: LoginState
  , trusts :: TrustState
  , debug :: DebugState
  , landing :: LandingState
  -- | CirclesProtocolDef GetState
  )

type LandingState =
  { checkSessionResult :: LandingStateCheckSessionResult
  }


type LandingStateCheckSessionResult = RemoteData Unit Unit ErrLandingStateResolved Unit

type ErrLandingStateResolved = Variant
  ( errDecode :: JsonDecodeError
  , errReadStorage :: RequestPath
  , errApi :: ApiError
  , errNative :: NativeError
  , errUserNotFound :: UserNotFoundError
  , errInvalidUrl :: String
  )

type ErrLandingState = Env.ErrRestoreSession
  + ErrLoginTask
  + ()



initLanding :: forall v. Variant (landing :: LandingState | v)
initLanding =
  _landing
    { checkSessionResult: _notAsked unit }


type DebugState =
  { magicWords :: String
  }

-- init :: CirclesState
init :: forall v. Variant (infoGeneral :: UserData | v)
init =
  _infoGeneral
    { direction: D._forwards
    , username: ""
    , usernameApiResult: _notAsked unit
    , email: ""
    , emailApiResult: _notAsked unit
    , terms: false
    , privacy: false
    , privateKey: Nothing
    , submitResult: _notAsked unit
    }

initDebug :: forall v. Variant (debug :: DebugState | v)
initDebug =
  _debug
    { magicWords: "hockey middle idea enable forget case mountain sugar chronic income crouch critic venue giant tell marble rose scene prefer shoe cheap run print pigeon"
    }

--------------------------------------------------------------------------------
_infoGeneral :: forall a v. a -> Variant (infoGeneral :: a | v)
_infoGeneral = inj (Proxy :: _ "infoGeneral")

_askUsername :: forall a v. a -> Variant (askUsername :: a | v)
_askUsername = inj (Proxy :: _ "askUsername")

_askEmail :: forall a v. a -> Variant (askEmail :: a | v)
_askEmail = inj (Proxy :: _ "askEmail")

_infoSecurity :: forall a v. a -> Variant (infoSecurity :: a | v)
_infoSecurity = inj (Proxy :: _ "infoSecurity")

_magicWords :: forall a v. a -> Variant (magicWords :: a | v)
_magicWords = inj (Proxy :: _ "magicWords")

_submit :: forall a v. a -> Variant (submit :: a | v)
_submit = inj (Proxy :: _ "submit")

_debug :: forall a v. a -> Variant (debug :: a | v)
_debug = inj (Proxy :: _ "debug")

_landing :: forall a v. a -> Variant (landing :: a | v)
_landing = inj (Proxy :: _ "landing")
