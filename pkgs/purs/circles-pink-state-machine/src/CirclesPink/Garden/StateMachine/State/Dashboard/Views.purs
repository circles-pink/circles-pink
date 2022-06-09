module CirclesPink.Garden.StateMachine.State.Dashboard.Views
  ( DefaultView
  , ErrDashboardStateResolved
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

import CirclesCore (ApiError, NativeError, User, TrustNode)
import CirclesCore.Bindings (Balance)
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.State (DashboardState)
import CirclesPink.Garden.StateMachine.State.Dashboard (TrustState, initUntrusted, next)
import CirclesPink.Garden.StateMachine.State.Dashboard as D
import Convertable (convert)
import Data.Array (any)
import Data.Map (lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.String (toLower)
import Data.Tuple (uncurry)
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant, default, onMatch)
import Foreign.Object (Object, values)
import Foreign.Object as O
import Network.Ethereum.Core.Signatures as W3
import RemoteData (RemoteData, isLoading)
import RemoteReport (RemoteReport)
import Wallet.PrivateKey (addrToString)

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
  { trusts :: Trusts
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
  }

type Trusts = Array Trust

type Trust =
  { safeAddress :: String
  , trustState :: TrustState
  , isOutgoing :: Boolean
  , user :: Nullable User
  }

mapTrust :: W3.Address -> D.Trust -> Trust
mapTrust a t =
  { isOutgoing: t.isOutgoing
  , trustState: t.trustState
  , safeAddress: show a
  , user: toNullable t.user
  }

mapTrusts :: D.Trusts -> Trusts
mapTrusts xs = M.toUnfoldable xs <#> uncurry mapTrust

defaultView :: DashboardState -> DefaultView
defaultView d@{ trusts, trustAddResult } =
  let
    initTrust user =
      { isOutgoing: false
      , user: Just user
      , trustState:
          O.lookup (toLower $ addrToString user.safeAddress) trustAddResult
            # maybe initUntrusted
                ( unwrap >>>
                    ( default initUntrusted # onMatch
                        { loading: \_ -> next initUntrusted
                        , success: \_ -> next $ next initUntrusted
                        }
                    )
                )

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
          ( \user -> lookup (convert user.safeAddress) trusts
              # maybe (initTrust user) identity
              # mapTrust (convert user.safeAddress)
          )
  in
    { trusts: mapTrusts trusts
    , usersSearch
    , userSearchResult: d.userSearchResult
    , getUsersResult: d.getUsersResult
    , trustsResult: d.trustsResult
    , trustAddResult: d.trustAddResult
    , trustRemoveResult: d.trustRemoveResult
    , checkUBIPayoutResult: d.checkUBIPayoutResult
    , getBalanceResult: d.getBalanceResult
    , requestUBIPayoutResult: d.requestUBIPayoutResult
    , transferResult: d.transferResult
    }

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

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type RemoteData_ e a = RemoteData Unit Unit e a

type RemoteReportV e a = RemoteReport (Variant e) a
