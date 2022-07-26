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
  , Graph
  , RemoteData_
  , Trust
  , Trusts
  , addrToString
  , defaultView
  , globalLoading
  , parseAddress
  ) where

import Prelude

import CirclesCore (ApiError, NativeError, SafeStatus, User)
import CirclesCore as CC
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.Address as A
import CirclesPink.Data.TrustConnection (TrustConnection(..), TsTrustConnection)
import CirclesPink.Data.TrustNode (TrustNode, userIdent)
import CirclesPink.Data.TrustState (TrustState, initUntrusted, isTrusted)
import CirclesPink.Data.UserIdent (UserIdent(..))
import CirclesPink.Garden.StateMachine.Control.EnvControl (UserNotFoundError)
import CirclesPink.Garden.StateMachine.State (DashboardState)
import CirclesPink.Garden.StateMachine.State.Dashboard (CirclesGraph)
import CirclesPink.Garden.StateMachine.ViewUtils (nubRemoteReport)
import Data.Array (any, filter)
import Data.BN (BN)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.FpTs.Option as FpTs
import Data.FpTs.Pair (Pair) as FPT
import Data.FpTs.Tuple (type (/\)) as FPT
import Data.IxGraph (getIndex)
import Data.IxGraph as G
import Data.Lens (view)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Pair ((~))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, default, onMatch)
import Foreign.Object (Object, values)
import FpTs.Class (toFpTs)
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
  , graph :: Graph
  , usersSearch :: Trusts
  , userSearchResult :: RemoteReport ErrUserSearchResolved (Array User)
  , getUsersResult :: RemoteData_ ErrGetUsersResolved (Array User)
  , trustAddResult :: Object (RemoteReport ErrTrustAddConnectionResolved String)
  , trustRemoveResult :: Object (RemoteReport ErrTrustRemoveConnectionResolved String)
  , trustsResult :: RemoteReport ErrTrustGetTrustsResolved (Array CC.TrustNode)
  , getBalanceResult :: RemoteReport ErrTokenGetBalanceResolved BN
  , checkUBIPayoutResult :: RemoteReport ErrTokenCheckUBIPayoutResolved BN
  , requestUBIPayoutResult :: RemoteReport ErrTokenRequestUBIPayoutResolved String
  , transferResult :: RemoteData_ ErrTokenTransferResolved String
  , redeploySafeResult :: RemoteReport ErrDeploySafeResolved SafeStatus
  , redeployTokenResult :: RemoteReport ErrDeployTokenResolved String
  }

type Graph =
  { nodes :: Array (Address FPT./\ TrustNode)
  , edges :: Array (FPT.Pair Address FPT./\ TsTrustConnection)
  }

type Trusts = Array Trust

type Trust =
  { trustState :: TrustState
  , isOutgoing :: Boolean
  , user :: UserIdent
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
          ( \user -> case trusts # G.lookupNode user.safeAddress of
              Left _ -> initUntrust user
              Right _ ->
                let
                  eitherIncomingEdge = trusts # G.lookupEdge (user.safeAddress ~ d.user.safeAddress)
                  eitherOutgoingEdge = trusts # G.lookupEdge (d.user.safeAddress ~ user.safeAddress)
                in
                  case eitherIncomingEdge, eitherOutgoingEdge of
                    Left _, Left _ -> initUntrust user
                    Right _, Left _ -> { isOutgoing: true, user: UserIdent $ Right user, trustState: initUntrusted }
                    Left _, Right (TrustConnection _ ts) -> { isOutgoing: false, user: UserIdent $ Right user, trustState: ts }
                    Right _, Right (TrustConnection _ ts) -> { isOutgoing: true, user: UserIdent $ Right user, trustState: ts }
          )

    trustsConfirmed = d.trusts
      # G.neighborEdgesWithNodes d.user.safeAddress
      # fold
      # filter (\((TrustConnection _ e) /\ _) -> isTrusted e)
      <#> (mapTrust' d.user.safeAddress d.trusts)

    trustsCandidates = d.trusts
      # G.outgoingEdgesWithNodes d.user.safeAddress
      # fold
      # filter (\((TrustConnection _ e) /\ _) -> not $ isTrusted e)
      <#> (mapTrust' d.user.safeAddress d.trusts)
  in
    { trustsConfirmed
    , trustsCandidates
    , graph: d.trusts # (G.toUnfoldables :: _ -> { nodes :: Array _, edges :: Array _ }) # toFpTs
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

mapTrust' :: Address -> CirclesGraph -> (TrustConnection /\ TrustNode) -> Trust
mapTrust' ownAddress graph (TrustConnection _ trustState /\ tn) =
  { trustState
  , isOutgoing: G.lookupEdge (getIndex tn ~ ownAddress) graph # either (const false) (const true)
  , user: view userIdent tn
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

addrToString :: Address -> String
addrToString = show

parseAddress :: String -> FpTs.Option Address
parseAddress = A.parseAddress >>> toFpTs