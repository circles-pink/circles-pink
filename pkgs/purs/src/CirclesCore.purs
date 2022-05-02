module CirclesCore
  ( Err
  , ErrApi
  , ErrInvalidUrl
  , ErrNative
  , ErrNewCirclesCore
  , ErrNewWebSocketProvider
  , ErrPrivKeyToAccount
  , ErrSafeDeploy
  , ErrSafeGetSafeStatus
  , ErrSafeIsFunded
  , ErrSafePredictAddress
  , ErrService
  , ErrTokenCheckUBIPayout
  , ErrTokenDeploy
  , ErrTokenGetBalance
  , ErrTokenRequestUBIPayout
  , ErrTrustAddConnection
  , ErrTrustGetNetwork
  , ErrTrustIsTrusted
  , ErrUserRegister
  , ErrUserResolve
  , NativeError
  , ResolveOptions
  , SafeDeployOptions
  , SafeStatus
  , TokenCheckUBIPayoutOptions
  , TokenDeployOptions
  , TokenGetBalanceOptions
  , TokenRequestUBIPayoutOptions
  , TrustAddConnectionOptions
  , TrustNode
  , User
  , UserOptions
  , _errApi
  , _errInvalidUrl
  , _errNative
  , _errService
  , module Exp
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , printErr
  , privKeyToAccount
  , safeDeploy
  , safeGetSafeStatus
  , safeIsFunded
  , safePredictAddress
  , safePrepareDeploy
  , sendTransaction
  , tokenCheckUBIPayout
  , tokenDeploy
  , tokenGetBalance
  , tokenRequestUBIPayout
  , trustAddConnection
  , trustGetNetwork
  , trustIsTrusted
  , unsafeSampleCore
  , userRegister
  , userResolve
  ) where

--------------------------------------------------------------------------------
-- Re-Exports
--------------------------------------------------------------------------------
import CirclesCore.Bindings
  ( Options
  , Provider
  , Web3
  , CirclesCore
  , Account
  , TrustIsTrustedResult
  , Balance
  )
  as Exp
import CirclesCore.ApiResult (ApiError) as Exp
--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------
import Prelude
import CirclesCore.ApiResult (apiResultToEither, ApiError)
import CirclesCore.Bindings (convertCore)
import CirclesCore.Bindings as B
import CirclesCore.FfiUtils (mapFn2)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Variant (Variant, case_, inj, on)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Exception (Error, message, name, try)
import Foreign (Foreign)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (Address, Nonce, PrivateKey, addrToString, nonceToBigInt)
import Wallet.PrivateKey as P

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
type Result e a
  = ExceptV e Aff a

type ErrNewWebSocketProvider r
  = ErrNative + ErrInvalidUrl + r

newWebSocketProvider :: forall r. String -> ExceptV (ErrNewWebSocketProvider r) Effect B.Provider
newWebSocketProvider x1 =
  B.newWebSocketProvider x1
    # try
    <#> lmap evalError
    # ExceptT
  where
  evalError e = case name e, message e of
    "TypeError", "Invalid URL" -> _errInvalidUrl x1
    _, _ -> mkErrorNative e

newWeb3 :: B.Provider -> Effect B.Web3
newWeb3 = B.newWeb3

type ErrNewCirclesCore r
  = ErrNative + r

newCirclesCore :: forall r. B.Web3 -> B.Options -> ExceptV (ErrNewCirclesCore r) Effect B.CirclesCore
newCirclesCore x1 x2 =
  B.newCirclesCore x1 x2
    # try
    <#> lmap mkErrorNative
    # ExceptT

type ErrPrivKeyToAccount r
  = ErrNative + r

privKeyToAccount :: forall r. B.Web3 -> PrivateKey -> ExceptV (ErrPrivKeyToAccount r) Effect B.Account
privKeyToAccount w3 pk =
  B.privKeyToAccount w3 (P.toString pk)
    # try
    <#> lmap mkErrorNative
    # ExceptT

sendTransaction :: forall r. B.Web3 -> Address -> Address -> ExceptV (ErrPrivKeyToAccount r) Effect Unit
sendTransaction w3 f t =
  B.sendTransaction w3 (P.addrToString f) (P.addrToString t)
    # try
    <#> lmap mkErrorNative
    # ExceptT

--------------------------------------------------------------------------------
-- API / trustIsTrusted
--------------------------------------------------------------------------------
type TrustIsTrustedOptions
  = { safeAddress :: Address
    , limit :: Int
    }

type ErrTrustIsTrusted r
  = ErrNative + r

trustIsTrusted :: forall r. B.CirclesCore -> B.Account -> TrustIsTrustedOptions -> Result (ErrTrustIsTrusted r) B.TrustIsTrustedResult
trustIsTrusted cc = mapFn2 (convertCore cc).trust.isTrusted pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / trustGetNetwork
--------------------------------------------------------------------------------
type TrustNode
  = { isIncoming :: Boolean
    , isOutgoing :: Boolean
    , limitPercentageIn :: Int
    , limitPercentageOut :: Int
    , mutualConnections :: Array Foreign
    , safeAddress :: Address
    }

type ErrTrustGetNetwork r
  = ErrNative + r

trustGetNetwork :: forall r. B.CirclesCore -> B.Account -> { safeAddress :: P.Address } -> Result (ErrTrustGetNetwork r) (Array TrustNode)
trustGetNetwork cc = mapFn2 (convertCore cc).trust.getNetwork pure (mapArg2 >>> pure) mkErrorNative (mapOk >>> pure)
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

  mapOk = map mapTrustNode

  mapTrustNode tn = tn { safeAddress = P.unsafeAddrFromString tn.safeAddress }

--------------------------------------------------------------------------------
-- API / trustAddConnection
--------------------------------------------------------------------------------
type TrustAddConnectionOptions
  = { user :: Address
    , canSendTo :: Address
    }

type ErrTrustAddConnection r
  = ErrNative + r

trustAddConnection :: forall r. B.CirclesCore -> B.Account -> TrustAddConnectionOptions -> Result (ErrTrustAddConnection r) String
trustAddConnection cc = mapFn2 (convertCore cc).trust.addConnection pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { user = addrToString x.user, canSendTo = addrToString x.canSendTo }

--------------------------------------------------------------------------------
-- API / userRegister
--------------------------------------------------------------------------------
type UserOptions
  = { nonce :: Nonce
    , safeAddress :: Address
    , username :: String
    , email :: String
    }

type ErrUserRegister r
  = ErrService + ErrNative + r

userRegister :: forall r. B.CirclesCore -> B.Account -> UserOptions -> Result (ErrUserRegister r) Unit
userRegister cc = mapFn2 (convertCore cc).user.register pure (mapArg2 >>> pure) mkErrorNative mapBoolean
  where
  mapArg2 x =
    x
      { nonce = nonceToBigInt x.nonce
      , safeAddress = addrToString x.safeAddress
      }

--------------------------------------------------------------------------------
-- API / userResolve
--------------------------------------------------------------------------------
type ResolveOptions
  = { addresses :: Array Address
    , userNames :: Array String
    }

type User
  = { id :: Int
    , username :: String
    , safeAddress :: Address
    , avatarUrl :: String
    }

type ErrUserResolve r
  = ErrNative + ErrApi + r

userResolve :: forall r. B.CirclesCore -> B.Account -> ResolveOptions -> Result (ErrUserResolve r) (Array User)
userResolve cc = mapFn2 (convertCore cc).user.resolve pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  mapArg2 x =
    x
      { addresses = map addrToString x.addresses
      , userNames = x.userNames
      }

  mapOk x = case apiResultToEither x of
    Left apiError -> Left $ _errApi apiError
    Right data_ -> pure $ map userToUser data_

  userToUser x = x { safeAddress = P.unsafeAddrFromString x.safeAddress }

--------------------------------------------------------------------------------
-- API / safeDeploy
--------------------------------------------------------------------------------
type SafeDeployOptions
  = { safeAddress :: Address
    }

type ErrSafeDeploy r
  = ErrService + ErrNative + r

safeDeploy :: forall r. B.CirclesCore -> B.Account -> SafeDeployOptions -> Result (ErrSafeDeploy r) Unit
safeDeploy cc = mapFn2 (convertCore cc).safe.deploy pure (mapArg2 >>> pure) mkErrorNative mapBoolean
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / safePredictAddress
--------------------------------------------------------------------------------
type ErrSafePredictAddress r
  = ErrNative + r

safePredictAddress :: forall r. B.CirclesCore -> B.Account -> { nonce :: P.Nonce } -> Result (ErrSafePredictAddress r) P.Address
safePredictAddress cc = mapFn2 (convertCore cc).safe.predictAddress pure (mapArg2 >>> pure) mkErrorNative (mapOk >>> pure)
  where
  mapArg2 x = x { nonce = P.nonceToBigInt x.nonce }

  mapOk = P.unsafeAddrFromString

--------------------------------------------------------------------------------
-- API / safePrepareDeploy
--------------------------------------------------------------------------------
type ErrSafePrepareDeploy r
  = ErrNative + r

safePrepareDeploy :: forall r. B.CirclesCore -> B.Account -> { nonce :: P.Nonce } -> Result (ErrSafePrepareDeploy r) P.Address
safePrepareDeploy cc = mapFn2 (convertCore cc).safe.prepareDeploy pure (mapArg2 >>> pure) mkErrorNative (mapOk >>> pure)
  where
  mapArg2 x = x { nonce = P.nonceToBigInt x.nonce }

  mapOk = P.unsafeAddrFromString

--------------------------------------------------------------------------------
-- API / safeIsFunded
--------------------------------------------------------------------------------
type SafeIsFundedOptions
  = { safeAddress :: Address
    }

type ErrSafeIsFunded r
  = ErrNative + r

safeIsFunded :: forall r. B.CirclesCore -> B.Account -> SafeIsFundedOptions -> Result (ErrSafeIsFunded r) Boolean
safeIsFunded cc = mapFn2 (convertCore cc).safe.isFunded pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / safeGetSafeStatus
--------------------------------------------------------------------------------
type SafeGetSafeStatusOptions
  = { safeAddress :: Address
    }

type SafeStatus
  = { isCreated :: Boolean
    , isDeployed :: Boolean
    }

type ErrSafeGetSafeStatus r
  = ErrNative + r

safeGetSafeStatus :: forall r. B.CirclesCore -> B.Account -> SafeGetSafeStatusOptions -> Result (ErrSafeGetSafeStatus r) SafeStatus
safeGetSafeStatus cc = mapFn2 (convertCore cc).safe.getSafeStatus pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenDeploy
--------------------------------------------------------------------------------
type TokenDeployOptions
  = { safeAddress :: Address
    }

type ErrTokenDeploy r
  = ErrService + ErrNative + r

tokenDeploy :: forall r. B.CirclesCore -> B.Account -> TokenDeployOptions -> Result (ErrTokenDeploy r) String
tokenDeploy cc = mapFn2 (convertCore cc).token.deploy pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenGetBalance
--------------------------------------------------------------------------------
type TokenGetBalanceOptions
  = { safeAddress :: Address
    }

type ErrTokenGetBalance r
  = ErrNative + r

tokenGetBalance :: forall r. B.CirclesCore -> B.Account -> TokenGetBalanceOptions -> Result (ErrTokenGetBalance r) B.Balance
tokenGetBalance cc = mapFn2 (convertCore cc).token.getBalance pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenCheckUBIPayout
--------------------------------------------------------------------------------
type TokenCheckUBIPayoutOptions
  = { safeAddress :: Address
    }

type ErrTokenCheckUBIPayout r
  = ErrNative + r

tokenCheckUBIPayout :: forall r. B.CirclesCore -> B.Account -> TokenCheckUBIPayoutOptions -> Result (ErrTokenCheckUBIPayout r) B.Balance
tokenCheckUBIPayout cc = mapFn2 (convertCore cc).token.checkUBIPayout pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenRequestUBIPayout
--------------------------------------------------------------------------------
type TokenRequestUBIPayoutOptions
  = { safeAddress :: Address
    }

type ErrTokenRequestUBIPayout r
  = ErrNative + r

tokenRequestUBIPayout :: forall r. B.CirclesCore -> B.Account -> TokenRequestUBIPayoutOptions -> Result (ErrTokenRequestUBIPayout r) String
tokenRequestUBIPayout cc = mapFn2 (convertCore cc).token.requestUBIPayout pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x = x { safeAddress = addrToString x.safeAddress }

--------------------------------------------------------------------------------
-- Err slices
--------------------------------------------------------------------------------
type ErrNative r
  = ( errNative :: NativeError | r )

type ErrInvalidUrl r
  = ( errInvalidUrl :: String | r )

type ErrApi r
  = ( errApi :: ApiError | r )

type ErrService r
  = ( errService :: Unit | r )

--------------------------------------------------------------------------------
-- Err constructors
--------------------------------------------------------------------------------
_errNative :: forall r. NativeError -> Variant (ErrNative r)
_errNative = inj (Proxy :: _ "errNative")

_errInvalidUrl :: forall r. String -> Variant (ErrInvalidUrl r)
_errInvalidUrl = inj (Proxy :: _ "errInvalidUrl")

_errApi :: forall r. ApiError -> Variant (ErrApi r)
_errApi = inj (Proxy :: _ "errApi")

_errService :: forall r. Unit -> Variant (ErrService r)
_errService = inj (Proxy :: _ "errService")

--------------------------------------------------------------------------------
-- Err composition
--------------------------------------------------------------------------------
type Err r
  = ErrNative + ErrService + ErrInvalidUrl + ErrApi + r

printErr :: Variant (Err ()) -> String
printErr =
  case_
    # on (Proxy :: _ "errNative") (\e -> "Native: " <> e.name <> ": " <> e.message)
    # on (Proxy :: _ "errService") (\_ -> "service error")
    # on (Proxy :: _ "errInvalidUrl") (\url -> "Invalid URL: " <> url)
    # on (Proxy :: _ "errApi") (\e -> "Api Error: " <> show e)

--------------------------------------------------------------------------------
-- Error types
--------------------------------------------------------------------------------
type NativeError
  = { message :: String
    , name :: String
    }

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
unsafeSampleCore :: forall r. B.CirclesCore -> B.Account -> Result (ErrNative + r) Unit
unsafeSampleCore x1 x2 =
  B.unsafeSampleCore x1 x2
    # fromEffectFnAff
    # attempt
    <#> lmap mkErrorNative
    # ExceptT

mkErrorNative :: forall r. Error -> Variant (ErrNative + r)
mkErrorNative e = _errNative { message: message e, name: name e }

mapBoolean :: forall r. Boolean -> Either (Variant (ErrService + r)) Unit
mapBoolean true = Right unit

mapBoolean false = Left $ _errService unit
