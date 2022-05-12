module CirclesPink.Garden.StateMachine.State.Dashboard.Views
  ( DefaultView
  , Trust
  , Trusts
  , defaultView
  ) where

import Prelude
import CirclesCore (User)
import CirclesPink.Garden.StateMachine.State (DashboardState)
import CirclesPink.Garden.StateMachine.State.Dashboard as D
import Data.Array (any)
import Data.Map as M
import Data.Nullable (Nullable, toNullable)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (values)
import Network.Ethereum.Core.Signatures as W3
import Record (merge)
import RemoteData (isLoading)
import Undefined (undefined)
import Wallet.PrivateKey (Address(..))

anythingLoading :: DashboardState -> Boolean
anythingLoading d = any (_ == true) $ join checks
  where
  check = isLoading

  checks =
    [ map check <$> values $ d.trustAddResult
    , map check <$> values $ d.trustRemoveResult
    , pure $ check d.trustsResult
    , pure $ check d.getBalanceResult
    , pure $ check d.checkUBIPayoutResult
    , pure $ check d.requestUBIPayoutResult
    , pure $ check d.getUsersResult
    , pure $ check d.transferResult
    , pure $ check d.userSearchResult
    ]

--------------------------------------------------------------------------------
type DefaultView
  = { trusts :: Trusts
    }

type Trusts
  = Array Trust

type Trust
  = { safeAddress :: String
    , isLoading :: Boolean
    , isIncoming :: Boolean
    , isOutgoing :: Boolean
    , user :: Nullable User
    }

mapTrust :: W3.Address /\ D.Trust -> Trust
mapTrust (a /\ t) =
  { isLoading: t.isLoading
  , isIncoming: t.isIncoming
  , isOutgoing: t.isOutgoing
  , safeAddress: show a
  , user: toNullable t.user
  }

mapTrusts :: D.Trusts -> Trusts
mapTrusts xs = M.toUnfoldable xs <#> mapTrust

defaultView :: DashboardState -> DefaultView
defaultView { trusts } =
  { trusts: mapTrusts trusts
  }

--------------------------------------------------------------------------------
