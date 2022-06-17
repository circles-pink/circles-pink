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
  , TrustEntry(..)
  , TrustGetTrusts
  , TrustRemoveResult
  , TrustState
  , Trusts
  , UserIdent
  , UserSearchResult
  , _dashboard
  , initDashboard
  , initTrusted
  , initUntrusted
  , isCandidate
  , isConfirmed
  , isLoadingTrust
  , isLoadingUntrust
  , isPendingTrust
  , isPendingUntrust
  , isTrusted
  , isUntrusted
  , matchTrustEntry
  , next
  , trustEntryToTrust
  )
  where
  
import Prelude

import CirclesCore (Balance, ErrInvalidUrl, ErrNative, ErrService, TrustNode, User)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Env as Env
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj, match)
import Foreign.Object (Object, empty)
import Network.Ethereum.Core.Signatures as W3
import Record as R
import RemoteData (RemoteData, _notAsked)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (Address, PrivateKey)

--------------------------------------------------------------------------------
-- DashboardState
--------------------------------------------------------------------------------
type DashboardState =
  { user :: CC.User
  , privKey :: PrivateKey
  , error :: Maybe (Variant (ErrDashboardState + ()))
  , trusts ::  Map W3.Address TrustEntry -- IxGraph W3.Address {} TrustEntry
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

--------------------------------------------------------------------------------

data TrustEntry = TrustCandidate Trust | TrustConfirmed Trust

isConfirmed :: TrustEntry -> Boolean
isConfirmed te = matchTrustEntry (\_ -> true) (\_ -> false) te

isCandidate :: TrustEntry -> Boolean
isCandidate te = matchTrustEntry (\_ -> false) (\_ -> true) te

matchTrustEntry :: forall z. (Trust -> z) -> (Trust -> z) -> TrustEntry -> z
matchTrustEntry confirmed candidate te = case te of
  TrustConfirmed t -> confirmed t
  TrustCandidate t -> candidate t

trustEntryToTrust :: TrustEntry -> Trust
trustEntryToTrust = matchTrustEntry identity identity

--------------------------------------------------------------------------------

type Trust =
  { isOutgoing :: Boolean
  , user :: Either W3.Address User
  , trustState :: TrustState
  }

type ErrDashboardState r = ErrService + ErrNative + ErrInvalidUrl + r

type UserIdent = Either Address User

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
type UserSearchResult = RemoteReport (Variant (ErrUserSearch + ())) (Array User)

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
newtype TrustState = TrustState
  ( Variant
      ( untrusted :: Unit -- 0
      , loadingTrust :: Unit -- 1
      , pendingTrust :: Unit -- 2
      , trusted :: Unit -- 3
      , loadingUntrust :: Unit -- 4
      , pendingUntrust :: Unit -- 5
      )
  )

derive instance trustStateEq :: Eq TrustState

derive newtype instance showTrustState :: Show TrustState

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
initTrusted :: TrustState
initTrusted = TrustState $ inj (Proxy :: _ "trusted") unit

initUntrusted :: TrustState
initUntrusted = TrustState $ inj (Proxy :: _ "untrusted") unit

--------------------------------------------------------------------------------
-- Destructors
--------------------------------------------------------------------------------
isUntrusted :: TrustState -> Boolean
isUntrusted (TrustState ts) = ts == (inj (Proxy :: _ "untrusted") unit)

isTrusted :: TrustState -> Boolean
isTrusted (TrustState ts) = ts == (inj (Proxy :: _ "trusted") unit)

isLoadingTrust :: TrustState -> Boolean
isLoadingTrust (TrustState ts) = ts == (inj (Proxy :: _ "loadingTrust") unit)

isLoadingUntrust :: TrustState -> Boolean
isLoadingUntrust (TrustState ts) = ts == (inj (Proxy :: _ "loadingUntrust") unit)

isPendingTrust :: TrustState -> Boolean
isPendingTrust (TrustState ts) = ts == (inj (Proxy :: _ "pendingTrust") unit)

isPendingUntrust :: TrustState -> Boolean
isPendingUntrust (TrustState ts) = ts == (inj (Proxy :: _ "pendingUntrust") unit)

next :: TrustState -> TrustState
next (TrustState ts) =
  TrustState
    $ match
        { untrusted: \_ -> inj (Proxy :: _ "loadingTrust") unit
        , loadingTrust: \_ -> inj (Proxy :: _ "pendingTrust") unit
        , pendingTrust: \_ -> inj (Proxy :: _ "trusted") unit
        , trusted: \_ -> inj (Proxy :: _ "loadingUntrust") unit
        , loadingUntrust: \_ -> inj (Proxy :: _ "pendingUntrust") unit
        , pendingUntrust: \_ -> inj (Proxy :: _ "untrusted") unit
        }
        ts
