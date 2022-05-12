module CirclesPink.Garden.StateMachine.State.Dashboard
  ( DashboardState
  , ErrDashboardStateResolved
  , ErrGetUsers
  , ErrGetUsersResolved
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
  , ErrTrustGetTrusts
  , ErrTrustGetTrustsResolved
  , ErrTrustRemoveConnection
  , ErrTrustRemoveConnectionResolved
  , ErrUserSearch
  , ErrUserSearchResolved
  , GetUsersResult
  , InitDashboard
  , TokenCheckUBIPayoutResult
  , TokenGetBalanceResult
  , TokenRequestUBIPayoutResult
  , TokenTransferResult
  , Trust
  , TrustAddResult
  , TrustGetTrusts
  , TrustRemoveResult
  , Trusts
  , UserSearchResult
  , _dashboard
  , initDashboard
  ) where

import Prelude
import CirclesCore (ApiError, Balance, NativeError, TrustNode, User)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Foreign.Object (Object, empty)
import Network.Ethereum.Core.Signatures as W3
import Record as R
import RemoteData (RemoteData, _notAsked)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (PrivateKey)

-- | Dashboard State
type ErrDashboardStateResolved
  = Variant
      ( errService :: Unit
      , errNative :: NativeError
      , errInvalidUrl :: String
      )

type Trusts
  = Map W3.Address Trust

type Trust
  = { isLoading :: Boolean
    , isIncoming :: Boolean
    , isOutgoing :: Boolean
    , user :: Maybe User
    }

type DashboardState
  = { user :: CC.User
    , privKey :: PrivateKey
    , error :: Maybe ErrDashboardStateResolved
    , trusts :: Trusts
    , trustsResult :: TrustGetTrusts
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
type InitDashboard
  = { user :: CC.User
    , privKey :: PrivateKey
    }

initDashboard :: InitDashboard -> forall v. Variant ( dashboard :: DashboardState | v )
initDashboard id =
  _dashboard
    $ R.disjointUnion id
        { error: Nothing
        , trusts: M.empty :: Map _ _
        , trustAddResult: empty :: Object _
        , trustRemoveResult: empty :: Object _
        , trustsResult: _notAsked unit
        , getBalanceResult: _notAsked unit
        , checkUBIPayoutResult: _notAsked unit
        , requestUBIPayoutResult: _notAsked unit
        , getUsersResult: _notAsked unit
        , transferResult: _notAsked unit
        , userSearchResult: _notAsked unit
        }

--------------------------------------------------------------------------------
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

-- Trust / GetTrusts
type ErrTrustGetTrustsResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTrustGetTrusts
  = Env.ErrAddTrustConnection + ()

type TrustGetTrusts
  = RemoteReport ErrTrustGetTrustsResolved (Array TrustNode)

-- | Trust / AddConnection
type ErrTrustAddConnectionResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTrustAddConnection
  = Env.ErrAddTrustConnection + ()

type TrustAddResult
  = Object (RemoteReport ErrTrustAddConnectionResolved String)

-- | Trust / RemoveConnection
type ErrTrustRemoveConnectionResolved
  = Variant
      ( errNative :: NativeError
      , errInvalidUrl :: String
      )

type ErrTrustRemoveConnection
  = Env.ErrRemoveTrustConnection + ()

type TrustRemoveResult
  = Object (RemoteReport ErrTrustRemoveConnectionResolved String)

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

--------------------------------------------------------------------------------
_dashboard :: forall a v. a -> Variant ( dashboard :: a | v )
_dashboard = inj (Proxy :: _ "dashboard")
