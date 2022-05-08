module CirclesPink.Garden.StateMachine.State.Dashboard.Views
  ( anythingLoading
  ) where

import Prelude
import CirclesPink.Garden.StateMachine.State (DashboardState)
import Data.Array (any)
import Foreign.Object (values)
import RemoteData (isLoading)

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
