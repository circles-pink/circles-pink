module CirclesPink.Garden.StateMachine.State.Dashboard.Views
  ( DefaultView
  , ErrDashboardStateResolved
  , ErrDeploySafeResolved
  , ErrDeployTokenResolved
  , ErrGetUsersResolved
  , ErrTokenCheckUBIPayoutResolved
  , ErrTokenGetBalanceResolved
  , ErrTokenRequestUBIPayoutResolved
  , ErrTokenTransferResolved
  , ErrTrustAddConnectionResolved
  , ErrTrustGetTrustsResolved
  , ErrTrustRemoveConnectionResolved
  , ErrUserSearchResolved
  , RemoteData_
  , Trust
  , Trusts
  , defaultView
  , globalLoading
  ) where

import Prelude

import CirclesCore (ApiError, NativeError, TrustNode, User, SafeStatus)
import CirclesCore.Bindings (Balance)
import CirclesPink.Data.Trust as T
import CirclesPink.Data.TrustEntry (TrustEntry, isCandidate, isConfirmed, trustEntryToTrust)
import CirclesPink.Data.TrustState (TrustState, initUntrusted)
import CirclesPink.Data.UserIdent (UserIdent)
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.State (DashboardState)
import CirclesPink.Garden.StateMachine.ViewUtils (nubRemoteReport)
import Convertable (convert)
import Data.Array (any)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.IxGraph as G
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Set as S
import Data.Variant (Variant, default, onMatch)
import Foreign.Object (Object, values)
import RemoteData (RemoteData, isLoading)
import RemoteReport (RemoteReport)

--------------------------------------------------------------------------------
-- globalLoading
--------------------------------------------------------------------------------
globalLoading :: DashboardState -> Boolean
globalLoading d = any (_ == true) $ join checks
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
-- DefaultView
--------------------------------------------------------------------------------
type DefaultView =
  { trustsConfirmed :: Trusts
  , trustsCandidates :: Trusts
  , usersSearch :: Trusts
  , userSearchResult :: RemoteReport ErrUserSearchResolved (Array User)
  , getUsersResult :: RemoteData_ ErrGetUsersResolved (Array User)
  , trustAddResult :: Object (RemoteReport ErrTrustAddConnectionResolved String)
  , trustRemoveResult :: Object (RemoteReport ErrTrustRemoveConnectionResolved String)
  , trustsResult :: RemoteReport ErrTrustGetTrustsResolved (Array TrustNode)
  , getBalanceResult :: RemoteReport ErrTokenGetBalanceResolved Balance
  , checkUBIPayoutResult :: RemoteReport ErrTokenCheckUBIPayoutResolved Balance
  , requestUBIPayoutResult :: RemoteReport ErrTokenRequestUBIPayoutResolved String
  , transferResult :: RemoteData_ ErrTokenTransferResolved String
  , redeploySafeResult :: RemoteReport ErrDeploySafeResolved SafeStatus
  , redeployTokenResult :: RemoteReport ErrDeployTokenResolved String
  }

type Trusts = Array Trust

type Trust =
  { trustState :: TrustState
  , isOutgoing :: Boolean
  , user :: UserIdent
  }

mapTrust :: T.Trust -> Trust
mapTrust t =
  { isOutgoing: t.isOutgoing
  , trustState: t.trustState
  , user: lmap convert t.user
  }

defaultView :: DashboardState -> DefaultView
defaultView d@{ trusts } =
  let
    initUntrust user =
      { isOutgoing: false
      , user: Right user
      , trustState: initUntrusted
      }

    usersSearch :: Trusts
    usersSearch =
      d.userSearchResult
        #
          ( unwrap >>>
              ( default [] # onMatch
                  { success: \{ data: data_ } -> data_
                  , loading: \{ previousData } -> maybe [] identity previousData
                  }
              )
          )
        <#>
          ( \user -> trusts
              # G.lookupNode (convert user.safeAddress)
              <#> trustEntryToTrust
              # maybe (initUntrust user) identity
              # mapTrust
          )
  in
    { trustsConfirmed: d.trusts # G.outgoingNodes (convert d.user.safeAddress) # maybe mempty identity # S.toUnfoldable # mapTrusts isConfirmed
    , trustsCandidates: d.trusts # G.outgoingNodes (convert d.user.safeAddress) # maybe mempty identity # S.toUnfoldable # mapTrusts isCandidate
    , usersSearch: usersSearch
    , userSearchResult: d.userSearchResult
    , getUsersResult: d.getUsersResult
    , trustsResult: d.trustsResult
    , trustAddResult: d.trustAddResult
    , trustRemoveResult: d.trustRemoveResult
    , checkUBIPayoutResult: d.checkUBIPayoutResult
    , getBalanceResult: d.getBalanceResult
    , requestUBIPayoutResult: d.requestUBIPayoutResult
    , transferResult: d.transferResult
    , redeploySafeResult: nubRemoteReport d.redeploySafeResult
    , redeployTokenResult: nubRemoteReport d.redeployTokenResult
    }

mapTrusts :: (TrustEntry -> Boolean) -> Array TrustEntry -> Trusts
mapTrusts pred xs = xs
  # A.filter pred
  # map
      ( \trustEntry ->
          let
            trust = trustEntryToTrust trustEntry
          in
            mapTrust trust
      )

--------------------------------------------------------------------------------
-- Resolved Errors
--------------------------------------------------------------------------------
type ErrUserSearchResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrGetUsersResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errInvalidUrl :: String
  , errUserNotFound :: UserNotFoundError
  )

type ErrTrustGetTrustsResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrTrustAddConnectionResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrTrustRemoveConnectionResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrTokenGetBalanceResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrTokenCheckUBIPayoutResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrTokenRequestUBIPayoutResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrTokenTransferResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrDashboardStateResolved = Variant
  ( errService :: Unit
  , errNative :: NativeError
  , errInvalidUrl :: String
  )

type ErrDeploySafeResolved = Variant
  ( errInvalidUrl :: String
  , errNative :: NativeError
  , errService :: Unit
  )

type ErrDeployTokenResolved = Variant
  ( errService :: Unit
  , errNative :: NativeError
  , errInvalidUrl :: String
  )

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type RemoteData_ e a = RemoteData Unit Unit e a

type RemoteReportV e a = RemoteReport (Variant e) a
