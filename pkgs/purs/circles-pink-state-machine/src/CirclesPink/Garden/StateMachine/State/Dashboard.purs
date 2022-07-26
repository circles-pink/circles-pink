module CirclesPink.Garden.StateMachine.State.Dashboard
  ( CirclesGraph
  , DashboardState
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
  , RedeploySafeResult
  , RedeployTokenResult
  , RemoteDataV_
  , RemoteReportV
  , TokenCheckUBIPayoutResult
  , TokenGetBalanceResult
  , TokenRequestUBIPayoutResult
  , TokenTransferResult
  , TrustAddResult
  , TrustGetTrusts
  , TrustRemoveResult
  , Trusts
  , UserSearchResult
  , _dashboard
  , initDashboard
  , module Exp
  )
  where

import Prelude

import CirclesCore (ErrInvalidUrl, ErrNative, ErrService, SafeStatus, User, ErrParseAddress)
import CirclesCore as CC
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Data.Trust (Trust)
import CirclesPink.Data.TrustConnection (TrustConnection)
import CirclesPink.Data.TrustNode (TrustNode)
import CirclesPink.Data.TrustNode (TrustNode) as Exp
import CirclesPink.Garden.StateMachine.Control.EnvControl as EnvControl
import Data.BN (BN)
import Data.IxGraph (IxGraph)
import Data.IxGraph as G
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Foreign.Object (Object, empty)
import Record as R
import RemoteData (RemoteData, _notAsked)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- DashboardState
--------------------------------------------------------------------------------
type DashboardState =
  { user :: CC.User
  , privKey :: PrivateKey
  , error :: Maybe (Variant (ErrDashboardState + ()))
  , trusts :: CirclesGraph
  , trustsResult :: TrustGetTrusts
  , trustAddResult :: TrustAddResult
  , trustRemoveResult :: TrustRemoveResult
  , getBalanceResult :: TokenGetBalanceResult
  , getUsersResult :: GetUsersResult
  , checkUBIPayoutResult :: TokenCheckUBIPayoutResult
  , requestUBIPayoutResult :: TokenRequestUBIPayoutResult
  , transferResult :: TokenTransferResult
  , userSearchResult :: UserSearchResult
  , redeploySafeResult :: RedeploySafeResult
  , redeployTokenResult :: RedeployTokenResult
  }

type Trusts = Map Address Trust

type CirclesGraph = IxGraph Address TrustConnection TrustNode

--------------------------------------------------------------------------------

type ErrDashboardState r = ErrService + ErrNative + ErrInvalidUrl + r

type RedeploySafeResult = RemoteReport
  (Variant (EnvControl.ErrDeploySafe + EnvControl.ErrGetSafeStatus + ()))
  SafeStatus

type RedeployTokenResult = RemoteReport
  (Variant (EnvControl.ErrDeployToken + ()))
  String

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
        , trusts: G.empty :: IxGraph _ _ _
        , trustAddResult: empty :: Object _
        , trustRemoveResult: empty :: Object _
        , trustsResult: _notAsked unit
        , getBalanceResult: _notAsked unit
        , checkUBIPayoutResult: _notAsked unit
        , requestUBIPayoutResult: _notAsked unit
        , getUsersResult: _notAsked unit
        , transferResult: _notAsked unit
        , userSearchResult: _notAsked unit
        , redeploySafeResult: _notAsked unit
        , redeployTokenResult: _notAsked unit
        }

--------------------------------------------------------------------------------
-- GetUsersResult
--------------------------------------------------------------------------------
type GetUsersResult = RemoteDataV_ (ErrGetUsers + ()) (Array User)

type ErrGetUsers r = EnvControl.ErrGetUsers + r

--------------------------------------------------------------------------------
-- UserSearchResult
--------------------------------------------------------------------------------
type UserSearchResult = RemoteReport (Variant (ErrUserSearch + ())) (Array User)

type ErrUserSearch r = EnvControl.ErrUserSearch + r

--------------------------------------------------------------------------------
-- TrustGetTrusts
--------------------------------------------------------------------------------
type TrustGetTrusts = RemoteReportV (ErrTrustGetTrusts + ()) (Array CC.TrustNode)

type ErrTrustGetTrusts r = EnvControl.ErrAddTrustConnection + ErrParseAddress + r

--------------------------------------------------------------------------------
-- TrustAddResult
--------------------------------------------------------------------------------
type TrustAddResult = Object (RemoteReportV (ErrTrustAddConnection + ()) String)

type ErrTrustAddConnection r = EnvControl.ErrAddTrustConnection + r

--------------------------------------------------------------------------------
-- TrustRemoveResult
--------------------------------------------------------------------------------
type TrustRemoveResult = Object (RemoteReportV (ErrTrustRemoveConnection + ()) String)

type ErrTrustRemoveConnection r = EnvControl.ErrRemoveTrustConnection + r

--------------------------------------------------------------------------------
-- TokenGetBalanceResult
--------------------------------------------------------------------------------
type TokenGetBalanceResult = RemoteReportV (ErrTokenGetBalance + ()) BN

type ErrTokenGetBalance r = EnvControl.ErrGetBalance + r

--------------------------------------------------------------------------------
-- TokenCheckUBIPayoutResult
--------------------------------------------------------------------------------
type TokenCheckUBIPayoutResult = RemoteReportV (ErrTokenCheckUBIPayout + ()) BN

type ErrTokenCheckUBIPayout r = EnvControl.ErrCheckUBIPayout + r

--------------------------------------------------------------------------------
-- TokenRequestUBIPayoutResult
--------------------------------------------------------------------------------
type TokenRequestUBIPayoutResult = RemoteReportV (ErrTokenRequestUBIPayout + ()) String

type ErrTokenRequestUBIPayout r = EnvControl.ErrRequestUBIPayout + r

--------------------------------------------------------------------------------
-- TokenTransferResult
--------------------------------------------------------------------------------
type TokenTransferResult = RemoteDataV_ (ErrTokenTransfer + ()) String

type ErrTokenTransfer r = EnvControl.ErrTransfer + r

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
