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
import CirclesPink.Data.TrustState (TrustState, initTrusted, initUntrusted, isTrusted)
import CirclesPink.Data.UserIdent (UserIdent(..))
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.State (DashboardState)
import CirclesPink.Garden.StateMachine.ViewUtils (nubRemoteReport)
import Data.Array (any, filter)
import Data.Either (Either(..))
import Data.IxGraph (IxGraph, getIndex)
import Data.IxGraph as G
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, default, onMatch)
import Foreign.Object (Object, values)
import Network.Ethereum.Core.Signatures (Address)
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
  , user: t.user
  }

defaultView :: DashboardState -> DefaultView
defaultView d@{ trusts } =
  let
    initUntrust user =
      { isOutgoing: false
      , user: UserIdent $ Right user
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
          ( \user -> mapTrust $ case trusts # G.lookupNode user.safeAddress of
              Nothing -> initUntrust user
              Just _ -> case trusts # G.lookupEdge user.safeAddress d.user.safeAddress, trusts # G.lookupEdge d.user.safeAddress user.safeAddress of
                Nothing, Nothing -> initUntrust user
                Just _, Nothing -> { isOutgoing: true, user: UserIdent $ Right user, trustState: initUntrusted }
                Nothing, Just _ -> { isOutgoing: false, user: UserIdent $ Right user, trustState: initTrusted }
                Just _, Just _ -> { isOutgoing: true, user: UserIdent $ Right user, trustState: initTrusted }
          )
  in
    { trustsConfirmed: d.trusts # G.outgoingEdgesWithNodes d.user.safeAddress # filter (\(e /\ _) -> isTrusted e) <#> (mapTrust' d.user.safeAddress d.trusts)
    , trustsCandidates: d.trusts # G.outgoingEdgesWithNodes d.user.safeAddress # filter (\(e /\ _) -> not $ isTrusted e) <#> (mapTrust' d.user.safeAddress d.trusts)
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

mapTrust' :: Address -> IxGraph Address TrustState UserIdent -> (TrustState /\ UserIdent) -> Trust
mapTrust' ownAddress graph (trustState /\ user) =
  { trustState
  , isOutgoing: G.lookupEdge (getIndex user) ownAddress graph # maybe false (const true)
  , user
  }

--------------------------------------------------------------------------------
-- Resolved Errors
--------------------------------------------------------------------------------
type ErrUserSearchResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errInvalidUrl :: String
  , errParseAddress :: String
  )

type ErrGetUsersResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errInvalidUrl :: String
  , errUserNotFound :: UserNotFoundError
  , errParseAddress :: String
  )

type ErrTrustGetTrustsResolved = Variant
  ( errNative :: NativeError
  , errInvalidUrl :: String
  , errParseAddress :: String
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
  , errParseAddress :: String
  )

type ErrDeployTokenResolved = Variant
  ( errService :: Unit
  , errNative :: NativeError
  , errInvalidUrl :: String
  , errParseAddress :: String
  )

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type RemoteData_ e a = RemoteData Unit Unit e a

type RemoteReportV e a = RemoteReport (Variant e) a
