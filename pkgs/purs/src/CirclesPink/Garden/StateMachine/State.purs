module CirclesPink.Garden.StateMachine.State
  ( AskEmailState
  , AskUsernameState
  , CirclesState
  , DebugState
  , EmailApiResult
  , ErrLoginState
  , ErrLoginStateResolved
  , ErrSubmit
  , ErrSubmitResolved
  , ErrTrustState
  , ErrTrustStateResolved
  , InfoGeneralState
  , InfoSecurityState
  , LoginState
  , LoginStateLoginResult
  , MagicWordsState
  , SubmitState
  , TrustState
  , TrustStateTrustsResult
  , UserData
  , UserDataSubmitResult
  , UsernameApiResult
  , _askEmail
  , _askUsername
  , _debug
  , _infoGeneral
  , _infoSecurity
  , _login
  , _magicWords
  , _submit
  , _trusts
  , init
  , initDebug
  , initLogin
  , module Exp
  ) where

--------------------------------------------------------------------------------
-- Re-exports
--------------------------------------------------------------------------------
import Prelude
import CirclesCore (ApiError, NativeError, SafeStatus, TrustNode)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.ProtocolDef (CirclesProtocolDef, GetState)
import CirclesPink.Garden.StateMachine.ProtocolDef (CirclesProtocolDef, GetAction, GetProtocol, GetState) as Exp
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask)
import CirclesPink.Garden.StateMachine.ProtocolDef.Common (ErrLoginTask) as Exp
import CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing (ErrLandingState, ErrLandingStateResolved, LandingAction, LandingState, LandingStateCheckSessionResult, LandingTransitions, _landing) as Exp
import CirclesPink.Garden.StateMachine.State.Dashboard (DashboardState)
import CirclesPink.Garden.StateMachine.State.Dashboard (DashboardState, ErrDashboardStateResolved, ErrGetUsers, ErrGetUsersResolved, ErrTokenCheckUBIPayout, ErrTokenCheckUBIPayoutResolved, ErrTokenGetBalance, ErrTokenGetBalanceResolved, ErrTokenRequestUBIPayout, ErrTokenRequestUBIPayoutResolved, ErrTokenTransfer, ErrTokenTransferResolved, ErrTrustAddConnection, ErrTrustAddConnectionResolved, ErrTrustGetTrusts, ErrTrustGetTrustsResolved, ErrTrustRemoveConnection, ErrTrustRemoveConnectionResolved, ErrUserSearch, GetUsersResult, InitDashboard, TokenCheckUBIPayoutResult, TokenGetBalanceResult, TokenRequestUBIPayoutResult, TokenTransferResult, TrustAddResult, TrustGetTrusts, TrustRemoveResult, _dashboard, initDashboard) as Exp
import Data.Maybe (Maybe(..))
import CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing (LandingState)
import Data.Variant (Variant, inj)
import RemoteData (RemoteData, _notAsked)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey as P
import Wallet.PrivateKey (PrivateKey)

type UsernameApiResult
  = RemoteData Unit Unit CirclesError { isValid :: Boolean }

type EmailApiResult
  = RemoteData Unit Unit CirclesError { isValid :: Boolean }

--------------------------------------------------------------------------------
-- UserData
--------------------------------------------------------------------------------
type ErrSubmit
  = Env.ErrGetSafeAddress
      + Env.ErrPrepareSafeDeploy
      + Env.ErrUserRegister
      + Env.ErrUserResolve
      + Env.ErrTrustGetNetwork
      + Env.ErrGetSafeStatus
      + Env.ErrSaveSession
      + ()

type ErrSubmitResolved
  = Variant
      ( errApi :: ApiError
      , errInvalidUrl :: String
      , errNative :: NativeError
      , errService :: Unit
      , errUserNotFound :: UserNotFoundError
      , errSaveSession :: Unit
      )

type UserDataSubmitResult
  = RemoteData Unit Unit ErrSubmitResolved Unit

type UserData
  = { direction :: D.Direction
    , username :: String
    , usernameApiResult :: UsernameApiResult
    , email :: String
    , emailApiResult :: EmailApiResult
    , terms :: Boolean
    , privacy :: Boolean
    , privateKey :: Maybe PrivateKey
    , submitResult :: UserDataSubmitResult
    }

type InfoGeneralState
  = UserData

type AskUsernameState
  = UserData

type AskEmailState
  = UserData

type InfoSecurityState
  = UserData

type MagicWordsState
  = UserData

type SubmitState
  = UserData

--------------------------------------------------------------------------------
type ErrLoginStateResolved
  = Variant
      ( errApi :: ApiError
      , errNative :: NativeError
      , errUserNotFound :: UserNotFoundError
      , errInvalidUrl :: String
      , errSaveSession :: Unit
      )

type ErrLoginState
  = ErrLoginTask
      + Env.ErrSaveSession
      + ()

type LoginStateLoginResult
  = RemoteData Unit Unit ErrLoginStateResolved Unit

type LoginState
  = { magicWords :: String
    , loginResult :: LoginStateLoginResult
    }

--------------------------------------------------------------------------------
type ErrTrustState
  = Env.ErrGetSafeStatus + Env.ErrIsTrusted + Env.ErrIsFunded + Env.ErrDeploySafe + Env.ErrDeployToken + ()

type ErrTrustStateResolved
  = Variant
      ( errService :: Unit
      , errNative :: NativeError
      , errInvalidUrl :: String
      )

type TrustStateTrustsResult
  = RemoteData Unit Unit ErrTrustStateResolved Unit

type TrustState
  = { user :: CC.User
    , privKey :: P.PrivateKey
    , trusts :: Array TrustNode
    , safeStatus :: SafeStatus
    , isReady :: Boolean
    , trustsResult :: TrustStateTrustsResult
    }

--------------------------------------------------------------------------------
type CirclesState
  = Variant
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

type DebugState
  = { magicWords :: String
    }

-- init :: CirclesState
init :: forall v. Variant ( infoGeneral :: UserData | v )
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

initLogin :: forall v. Variant ( login :: LoginState | v )
initLogin =
  _login
    { magicWords: ""
    , loginResult: _notAsked unit
    }

initDebug :: forall v. Variant ( debug :: DebugState | v )
initDebug =
  _debug
    { magicWords: "hockey middle idea enable forget case mountain sugar chronic income crouch critic venue giant tell marble rose scene prefer shoe cheap run print pigeon"
    }

--------------------------------------------------------------------------------
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

_login :: forall a v. a -> Variant ( login :: a | v )
_login = inj (Proxy :: _ "login")

_trusts :: forall a v. a -> Variant ( trusts :: a | v )
_trusts = inj (Proxy :: _ "trusts")

_debug :: forall a v. a -> Variant ( debug :: a | v )
_debug = inj (Proxy :: _ "debug")
