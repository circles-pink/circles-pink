module CirclesPink.Garden.StateMachine.State
  ( AskEmailState
  , AskUsernameState
  , CirclesState
  , DashboardState
  , DebugState
  , EmailApiResult
  , ErrDashboardStateResolved
  , ErrGetUsers
  , ErrGetUsersResolved
  , ErrLandingState
  , ErrLandingStateResolved
  , ErrLoginState
  , ErrLoginStateResolved
  , ErrLoginTask
  , ErrSubmit
  , ErrSubmitResolved
  , ErrTokenCheckUBIPayout
  , ErrTokenCheckUBIPayoutResolved
  , ErrTokenGetBalance
  , ErrTokenGetBalanceResolved
  , ErrTokenRequestUBIPayout
  , ErrTokenRequestUBIPayoutResolved
  , ErrTokenTransfer
  , ErrTokenTransferResolved
  , ErrTrustAddConnection
  , ErrTrustAddConnectionResolved
  , ErrTrustRemoveConnection
  , ErrTrustRemoveConnectionResolved
  , ErrTrustState
  , ErrTrustStateResolved
  , ErrUserSearch
  , ErrUserSearchResolved
  , GetUsersResult
  , InfoGeneralState
  , InfoSecurityState
  , LandingState
  , LandingStateCheckSessionResult
  , LoginState
  , LoginStateLoginResult
  , MagicWordsState
  , SubmitState
  , TokenCheckUBIPayoutResult
  , TokenGetBalanceResult
  , TokenRequestUBIPayoutResult
  , TokenTransferResult
  , TrustAddResult
  , TrustRemoveResult
  , TrustState
  , TrustStateTrustsResult
  , UserData
  , UserDataSubmitResult
  , UserSearchResult
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
  , initDashboard
  , initDebug
  , initLanding
  , initLogin
  ) where

import Prelude
import CirclesCore (ApiError, Balance, NativeError, SafeStatus, TrustNode, User)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError, RequestPath)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import Data.Argonaut (JsonDecodeError)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Record as R
import RemoteData (RemoteData, _notAsked)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (PrivateKey)
import Wallet.PrivateKey as P

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
    , privateKey :: PrivateKey
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
type ErrLandingStateResolved
  = Variant
      ( errDecode :: JsonDecodeError
      , errReadStorage :: RequestPath
      , errApi :: ApiError
      , errNative :: NativeError
      , errUserNotFound :: UserNotFoundError
      , errInvalidUrl :: String
      )

type ErrLandingState
  = Env.ErrRestoreSession
      + ErrLoginTask
      + ()

type LandingStateCheckSessionResult
  = RemoteData Unit Unit ErrLandingStateResolved Unit

type LandingState
  = { checkSessionResult :: LandingStateCheckSessionResult
    }

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

type ErrLoginTask r
  = Env.ErrUserResolve
      + Env.ErrGetSafeStatus
      + Env.ErrTrustGetNetwork
      + Env.ErrIsTrusted
      + Env.ErrIsFunded
      + r

type LoginStateLoginResult
  = RemoteData Unit Unit ErrLoginStateResolved Unit

type LoginState
  = { magicWords :: String
    , loginResult :: LoginStateLoginResult
    }

--------------------------------------------------------------------------------
-- | User / getUsers
type ErrGetUsersResolved
  = Variant
      ( errApi :: ApiError
      , errNative :: NativeError
      , errInvalidUrl :: String
      , errUserNotFound :: UserNotFoundError
      )

type ErrGetUsers
  = Env.ErrGetUsers + ()

type GetUsersResult
  = RemoteData Unit Unit ErrGetUsersResolved (Array User)

--------------------------------------------------------------------------------
-- | User / search
type ErrUserSearchResolved
  = Variant
      ( errApi :: ApiError
      , errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrUserSearch
  = Env.ErrUserSearch + ()

type UserSearchResult
  = RemoteData Unit Unit ErrUserSearchResolved (Array User)

-- | Trust / AddConnection
type ErrTrustAddConnectionResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTrustAddConnection
  = Env.ErrAddTrustConnection + ()

type TrustAddResult
  = RemoteData Unit Unit ErrTrustAddConnectionResolved Unit

-- | Trust / RemoveConnection
type ErrTrustRemoveConnectionResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTrustRemoveConnection
  = Env.ErrRemoveTrustConnection + ()

type TrustRemoveResult
  = RemoteData Unit Unit ErrTrustRemoveConnectionResolved Unit

-- | Token / GetBalance
type ErrTokenGetBalanceResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTokenGetBalance
  = Env.ErrGetBalance + ()

type TokenGetBalanceResult
  = RemoteReport ErrTokenGetBalanceResolved Balance

-- | Token / CheckUBIPayout
type ErrTokenCheckUBIPayoutResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTokenCheckUBIPayout
  = Env.ErrCheckUBIPayout + ()

type TokenCheckUBIPayoutResult
  = RemoteReport ErrTokenCheckUBIPayoutResolved Balance

-- | Token / RequestUBIPayout
type ErrTokenRequestUBIPayoutResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTokenRequestUBIPayout
  = Env.ErrRequestUBIPayout + ()

type TokenRequestUBIPayoutResult
  = RemoteReport ErrTokenRequestUBIPayoutResolved String

-- | Token / Transfer
type ErrTokenTransferResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTokenTransfer
  = Env.ErrTransfer + ()

type TokenTransferResult
  = RemoteData Unit Unit ErrTokenTransferResolved String

-- | Dashboard State
type ErrDashboardStateResolved
  = Variant
      ( errService :: Unit
      , errNative :: NativeError
      , errInvalidUrl :: String
      )

type DashboardState
  = { user :: CC.User
    , privKey :: PrivateKey
    , trusts :: Array TrustNode
    , error :: Maybe ErrDashboardStateResolved
    , trustAddResult :: TrustAddResult
    , trustRemoveResult :: TrustRemoveResult
    , getBalanceResult :: TokenGetBalanceResult
    , getUsersResult :: GetUsersResult
    , checkUBIPayoutResult :: TokenCheckUBIPayoutResult
    , requestUBIPayoutResult :: TokenRequestUBIPayoutResult
    , transferResult :: TokenTransferResult
    , userSearchResult :: UserSearchResult
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
    , privKey :: PrivateKey
    , trusts :: Array TrustNode
    , safeStatus :: SafeStatus
    , isReady :: Boolean
    , trustsResult :: TrustStateTrustsResult
    }

--------------------------------------------------------------------------------
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
    , privateKey: P.zeroKey
    , submitResult: _notAsked unit
    }

initLanding :: forall v. Variant ( landing :: LandingState | v )
initLanding =
  _landing
    { checkSessionResult: _notAsked unit }

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
type InitDashboard
  = { user :: CC.User
    , privKey :: PrivateKey
    , trusts :: Array TrustNode
    }

initDashboard :: InitDashboard -> forall v. Variant ( dashboard :: DashboardState | v )
initDashboard id =
  _dashboard
    $ R.disjointUnion id
        { error: Nothing
        , trustAddResult: _notAsked unit :: RemoteData _ _ _ _
        , trustRemoveResult: _notAsked unit :: RemoteData _ _ _ _
        , getBalanceResult: _notAsked unit :: RemoteData _ _ _ _
        , checkUBIPayoutResult: _notAsked unit :: RemoteData _ _ _ _
        , requestUBIPayoutResult: _notAsked unit :: RemoteData _ _ _ _
        , getUsersResult: _notAsked unit :: RemoteData _ _ _ _
        , transferResult: _notAsked unit :: RemoteData _ _ _ _
        , userSearchResult: _notAsked unit :: RemoteData _ _ _ _
        }

--------------------------------------------------------------------------------
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
