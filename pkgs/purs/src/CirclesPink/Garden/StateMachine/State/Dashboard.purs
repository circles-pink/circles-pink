module CirclesPink.Garden.StateMachine.State.Dashboard
  ( DashboardState
  , ErrDashboardState
  , ErrGetUsers
  , ErrTokenCheckUBIPayout
  , ErrTokenGetBalance
  , ErrTokenRequestUBIPayout
  , ErrTokenTransfer
  , ErrTrustAddConnection
  , ErrTrustGetTrusts
  , ErrTrustRemoveConnection
  , ErrUserSearch
  , GetUsersResult
  , InitDashboard
  , RemoteDataV_
  , RemoteReportV
  , TokenCheckUBIPayoutResult
  , TokenGetBalanceResult
  , TokenRequestUBIPayoutResult
  , TokenTransferResult
  , Trust
  , TrustAddResult
  , TrustGetTrusts
  , TrustRemoveResult
  , TrustState
  , Trusts
  , UserSearchResult
  , _dashboard
  , _loadingTrust
  , _loadingUntrust
  , _pendingTrust
  , _pendingUntrust
  , _trusted
  , _untrusted
  , initDashboard
  ) where

import Prelude

import CirclesCore (Balance, ErrInvalidUrl, ErrNative, ErrService, TrustNode, User)
import CirclesCore as CC
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

--------------------------------------------------------------------------------
-- DashboardState
--------------------------------------------------------------------------------
type DashboardState =
  { user :: CC.User
  , privKey :: PrivateKey
  , error :: Maybe (Variant (ErrDashboardState + ()))
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

type Trusts = Map W3.Address Trust

type Trust =
  { isOutgoing :: Boolean
  , user :: Maybe User
  , trustState :: TrustState
  }

type ErrDashboardState r = ErrService + ErrNative + ErrInvalidUrl + r

--------------------------------------------------------------------------------
-- InitDashboard
--------------------------------------------------------------------------------
type InitDashboard =
  { user :: CC.User
  , privKey :: PrivateKey
  }

initDashboard :: InitDashboard -> forall v. Variant (dashboard :: DashboardState | v)
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
-- GetUsersResult
--------------------------------------------------------------------------------
type GetUsersResult = RemoteDataV_ (ErrGetUsers + ()) (Array User)

type ErrGetUsers r = Env.ErrGetUsers + r

--------------------------------------------------------------------------------
-- UserSearchResult
--------------------------------------------------------------------------------
type UserSearchResult = RemoteData Unit Unit (Variant (ErrUserSearch + ())) (Array User)

type ErrUserSearch r = Env.ErrUserSearch + r

--------------------------------------------------------------------------------
-- TrustGetTrusts
--------------------------------------------------------------------------------
type TrustGetTrusts = RemoteReportV (ErrTrustGetTrusts + ()) (Array TrustNode)

type ErrTrustGetTrusts r = Env.ErrAddTrustConnection + r

--------------------------------------------------------------------------------
-- TrustAddResult
--------------------------------------------------------------------------------
type TrustAddResult = Object (RemoteReportV (ErrTrustAddConnection + ()) String)

type ErrTrustAddConnection r = Env.ErrAddTrustConnection + r

--------------------------------------------------------------------------------
-- TrustRemoveResult
--------------------------------------------------------------------------------
type TrustRemoveResult = Object (RemoteReportV (ErrTrustRemoveConnection + ()) String)

type ErrTrustRemoveConnection r = Env.ErrRemoveTrustConnection + r

--------------------------------------------------------------------------------
-- TokenGetBalanceResult
--------------------------------------------------------------------------------
type TokenGetBalanceResult = RemoteReportV (ErrTokenGetBalance + ()) Balance

type ErrTokenGetBalance r = Env.ErrGetBalance + r

--------------------------------------------------------------------------------
-- TokenCheckUBIPayoutResult
--------------------------------------------------------------------------------
type TokenCheckUBIPayoutResult = RemoteReportV (ErrTokenCheckUBIPayout + ()) Balance

type ErrTokenCheckUBIPayout r = Env.ErrCheckUBIPayout + r

--------------------------------------------------------------------------------
-- TokenRequestUBIPayoutResult
--------------------------------------------------------------------------------
type TokenRequestUBIPayoutResult = RemoteReportV (ErrTokenRequestUBIPayout + ()) String

type ErrTokenRequestUBIPayout r = Env.ErrRequestUBIPayout + r

--------------------------------------------------------------------------------
-- TokenTransferResult
--------------------------------------------------------------------------------
type TokenTransferResult = RemoteDataV_ (ErrTokenTransfer + ()) String

type ErrTokenTransfer r = Env.ErrTransfer + r

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
_dashboard :: forall a v. a -> Variant (dashboard :: a | v)
_dashboard = inj (Proxy :: _ "dashboard")

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type RemoteDataV_ e a = RemoteData Unit Unit (Variant e) a

type RemoteReportV e a = RemoteReport (Variant e) a

--------------------------------------------------------------------------------

type TrustState = Variant
  ( loadingTrust :: Unit
  , loadingUntrust :: Unit
  , pendingTrust :: Unit
  , pendingUntrust :: Unit
  , trusted :: Unit
  , untrusted :: Unit
  )

_trusted :: TrustState
_trusted = inj (Proxy :: _ "trusted") unit

_untrusted :: TrustState
_untrusted = inj (Proxy :: _ "untrusted") unit

_loadingTrust :: TrustState
_loadingTrust = inj (Proxy :: _ "loadingTrust") unit

_loadingUntrust :: TrustState
_loadingUntrust = inj (Proxy :: _ "loadingUntrust") unit

_pendingTrust :: TrustState
_pendingTrust = inj (Proxy :: _ "pendingTrust") unit

_pendingUntrust :: TrustState
_pendingUntrust = inj (Proxy :: _ "pendingUntrust") unit
