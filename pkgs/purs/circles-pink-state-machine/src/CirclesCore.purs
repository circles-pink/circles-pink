module CirclesCore
  ( Err
  , ErrApi
  , ErrInvalidUrl
  , ErrNative
  , ErrNewCirclesCore
  , ErrNewWebSocketProvider
  , ErrNotGivenOrAllowed
  , ErrNullReturn
  , ErrOrganizationIsOrganization
  , ErrParseAddress
  , ErrPrivKeyToAccount
  , ErrSafeDeploy
  , ErrSafeGetSafeStatus
  , ErrSafeIsFunded
  , ErrSafePredictAddress
  , ErrSearch
  , ErrSendTransaction
  , ErrService
  , ErrTokenCheckUBIPayout
  , ErrTokenDeploy
  , ErrTokenGetBalance
  , ErrTokenRequestUBIPayout
  , ErrTokenTransfer
  , ErrTrustAddConnection
  , ErrTrustGetNetwork
  , ErrTrustIsTrusted
  , ErrTrustRemoveConnection
  , ErrUserRegister
  , ErrUserResolve
  , NativeError
  , Nonce(..)
  , OrganizationIsOrganizationOptions
  , ResolveOptions
  , SafeAddress(..)
  , SafeDeployOptions
  , SafeStatus
  , SearchOptions
  , TokenCheckUBIPayoutOptions
  , TokenDeployOptions
  , TokenGetBalanceOptions
  , TokenRequestUBIPayoutOptions
  , TokenTransferOptions
  , TrustAddConnectionOptions
  , TrustIsTrustedOptions
  , TrustNode
  , TrustRemoveConnectionOptions
  , User
  , UserOptions
  , UtilsRequestRelayerOptions
  , _errApi
  , _errInvalidUrl
  , _errNative
  , _errParseAddress
  , _errService
  , module Exp
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , organizationIsOrganization
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
  , tokenGetPaymentNote
  , tokenRequestUBIPayout
  , tokenTransfer
  , trustAddConnection
  , trustGetNetwork
  , trustIsTrusted
  , trustRemoveConnection
  , unsafeSampleCore
  , userRegister
  , userResolve
  , userSearch
  , utilsRequestRelayer
  )
  where

--------------------------------------------------------------------------------
-- Re-Exports
--------------------------------------------------------------------------------

import Prelude

import CirclesCore.ApiResult (ApiError) as Exp
import CirclesCore.ApiResult (apiResultToEither, ApiError)
import CirclesCore.Bindings (Options, Provider, CirclesCore, Account, TrustIsTrustedResult) as Exp
import CirclesCore.Bindings (convertCore)
import CirclesCore.Bindings as B
import CirclesCore.FfiUtils (mapFn1, mapFn2)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (ExceptT(..), throwError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.BN (BN)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.Either (Either(..), note)
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype.Extra ((-#))
import Data.Nullable as N
import Data.Traversable (traverse)
import Data.Variant (Variant, case_, inj, on)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Exception (Error, message, name, try)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (Address, PrivateKey, mkAddress)
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Untagged.Union as UU
import Web3 as W3

--------------------------------------------------------------------------------
-- Web3
--------------------------------------------------------------------------------
type Result e a = ExceptV e Aff a

type ErrNewWebSocketProvider r = ErrNative + ErrInvalidUrl + r

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

newWeb3 :: B.Provider -> Effect W3.Web3
newWeb3 = B.newWeb3

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
type ErrNewCirclesCore r = ErrNative + r

newCirclesCore :: forall r. W3.Web3 -> B.Options -> ExceptV (ErrNewCirclesCore r) Effect B.CirclesCore
newCirclesCore x1 x2 =
  B.newCirclesCore x1 x2
    # try
    <#> lmap mkErrorNative
    # ExceptT

--------------------------------------------------------------------------------
type ErrPrivKeyToAccount r = ErrNative + r

privKeyToAccount :: forall r. W3.Web3 -> PrivateKey -> ExceptV (ErrPrivKeyToAccount r) Effect B.Account
privKeyToAccount w3 pk =
  B.privKeyToAccount w3 (show pk)
    # try
    <#> lmap mkErrorNative
    # ExceptT

--------------------------------------------------------------------------------
type ErrSendTransaction r = ErrNative + r

sendTransaction :: forall r. W3.Web3 -> Address -> Address -> ExceptV (ErrSendTransaction r) Effect Unit
sendTransaction w3 f t =
  B.sendTransaction w3 (show f) (show t)
    # try
    <#> lmap mkErrorNative
    # ExceptT

--------------------------------------------------------------------------------
-- API / trustIsTrusted
--------------------------------------------------------------------------------
type TrustIsTrustedOptions =
  { safeAddress :: ChecksumAddress
  , limit :: Int
  }

type ErrTrustIsTrusted r = ErrNative + r

trustIsTrusted :: forall r. B.CirclesCore -> B.Account -> TrustIsTrustedOptions -> Result (ErrTrustIsTrusted r) B.TrustIsTrustedResult
trustIsTrusted cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.trust -# _.isTrusted

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / trustGetNetwork
--------------------------------------------------------------------------------
type TrustNode =
  { isIncoming :: Boolean
  , isOutgoing :: Boolean
  , limitPercentageIn :: Int
  , limitPercentageOut :: Int
  , mutualConnections :: Array Address
  , safeAddress :: Address
  }

type ErrTrustGetNetwork r = ErrNative + ErrParseAddress + r

trustGetNetwork :: forall r. B.CirclesCore -> B.Account -> { safeAddress :: ChecksumAddress } -> Result (ErrTrustGetNetwork r) (Array TrustNode)
trustGetNetwork cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.trust -# _.getNetwork

  mapArg2 x = x { safeAddress = show x.safeAddress }

  mapOk = traverse mapTrustNode

  mapTrustNode tn = do
    safeAddress <- parseAddr tn.safeAddress
    mutualConnections <- traverse parseAddr tn.mutualConnections
    pure tn
      { safeAddress = safeAddress
      , mutualConnections = mutualConnections
      }

--------------------------------------------------------------------------------
-- API / trustAddConnection
--------------------------------------------------------------------------------
type TrustAddConnectionOptions =
  { user :: ChecksumAddress
  , canSendTo :: ChecksumAddress
  , limitPercentage :: Number
  }

type ErrTrustAddConnection r = ErrNative + ErrNullReturn + r

trustAddConnection :: forall r. B.CirclesCore -> B.Account -> TrustAddConnectionOptions -> Result (ErrTrustAddConnection r) String
trustAddConnection cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.trust -# _.addConnection

  mapArg2 x = x
    { user = show x.user
    , canSendTo = show x.canSendTo
    , limitPercentage = x.limitPercentage
    }

  mapOk x = case UU.toEither1 x of
    Left str -> pure str
    Right _ -> throwError $ inj (Proxy :: _ "errNullReturn") unit

--------------------------------------------------------------------------------
-- API / trustRemoveConnection
--------------------------------------------------------------------------------
type TrustRemoveConnectionOptions =
  { user :: ChecksumAddress
  , canSendTo :: ChecksumAddress
  }

type ErrTrustRemoveConnection r = ErrNative + ErrNullReturn + r

trustRemoveConnection :: forall r. B.CirclesCore -> B.Account -> TrustRemoveConnectionOptions -> Result (ErrTrustRemoveConnection r) String
trustRemoveConnection cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.trust -# _.removeConnection

  mapArg2 x = x { user = show x.user, canSendTo = show x.canSendTo }

  mapOk x = case UU.toEither1 x of
    Left str -> pure str
    Right _ -> throwError $ inj (Proxy :: _ "errNullReturn") unit

--------------------------------------------------------------------------------
-- API / userRegister
--------------------------------------------------------------------------------
newtype Nonce = Nonce BigInt

derive instance Newtype Nonce _

type UserOptions =
  { nonce :: Nonce
  , safeAddress :: ChecksumAddress
  , username :: String
  , email :: String
  }

type ErrUserRegister r = ErrService + ErrNative + r

userRegister :: forall r. B.CirclesCore -> B.Account -> UserOptions -> Result (ErrUserRegister r) Unit
userRegister cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapBoolean
  where
  fn = convertCore cc -# _.user -# _.register

  mapArg2 x =
    x
      { nonce = unwrap x.nonce
      , safeAddress = show x.safeAddress
      }

--------------------------------------------------------------------------------
-- API / userResolve
--------------------------------------------------------------------------------
type ResolveOptions =
  { addresses :: Array ChecksumAddress
  , userNames :: Array String
  }

type User =
  { id :: Int
  , username :: String
  , safeAddress :: Address
  , avatarUrl :: String
  }

type ErrUserResolve r = ErrNative + ErrParseAddress + ErrApi + r

userResolve :: forall r. B.CirclesCore -> B.Account -> ResolveOptions -> Result (ErrUserResolve r) (Array User)
userResolve cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.user -# _.resolve

  mapArg2 x =
    x
      { addresses = map show x.addresses
      , userNames = x.userNames
      }

  mapOk x = case apiResultToEither x of
    Left apiError -> Left $ _errApi apiError
    Right data_ -> traverse userToUser data_

--------------------------------------------------------------------------------
-- API / userSearch
--------------------------------------------------------------------------------
type SearchOptions =
  { query :: String
  }

type ErrSearch r = ErrNative + ErrParseAddress + ErrApi + r

userSearch :: forall r. B.CirclesCore -> B.Account -> SearchOptions -> Result (ErrSearch r) (Array User)
userSearch cc = mapFn2 fn pure pure mkErrorNative mapOk
  where
  fn = convertCore cc -# _.user -# _.search

  mapOk x = case apiResultToEither x of
    Left apiError -> Left $ _errApi apiError
    Right data_ -> traverse userToUser data_

--------------------------------------------------------------------------------
-- API / safeDeploy
--------------------------------------------------------------------------------
type SafeDeployOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrSafeDeploy r = ErrService + ErrNative + r

safeDeploy :: forall r. B.CirclesCore -> B.Account -> SafeDeployOptions -> Result (ErrSafeDeploy r) Unit
safeDeploy cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapBoolean
  where
  fn = convertCore cc -# _.safe -# _.deploy

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / safePredictAddress
--------------------------------------------------------------------------------
type ErrSafePredictAddress r = ErrNative + ErrParseAddress + r

safePredictAddress :: forall r. B.CirclesCore -> B.Account -> { nonce :: Nonce } -> Result (ErrSafePredictAddress r) Address
safePredictAddress cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.safe -# _.predictAddress

  mapArg2 x = x { nonce = unwrap x.nonce }

  mapOk = parseAddr

--------------------------------------------------------------------------------
-- API / safePrepareDeploy
--------------------------------------------------------------------------------
type ErrSafePrepareDeploy r = ErrNative + ErrParseAddress + r

safePrepareDeploy :: forall r. B.CirclesCore -> B.Account -> { nonce :: Nonce } -> Result (ErrSafePrepareDeploy r) Address
safePrepareDeploy cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.safe -# _.prepareDeploy

  mapArg2 x = x { nonce = unwrap x.nonce }

  mapOk = parseAddr

--------------------------------------------------------------------------------
-- API / safeIsFunded
--------------------------------------------------------------------------------
type SafeIsFundedOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrSafeIsFunded r = ErrNative + r

safeIsFunded :: forall r. B.CirclesCore -> B.Account -> SafeIsFundedOptions -> Result (ErrSafeIsFunded r) Boolean
safeIsFunded cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.safe -# _.isFunded

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / safeGetSafeStatus
--------------------------------------------------------------------------------
type SafeGetSafeStatusOptions =
  { safeAddress :: ChecksumAddress
  }

type SafeStatus =
  { isCreated :: Boolean
  , isDeployed :: Boolean
  }

type ErrSafeGetSafeStatus r = ErrNative + r

safeGetSafeStatus :: forall r. B.CirclesCore -> B.Account -> SafeGetSafeStatusOptions -> Result (ErrSafeGetSafeStatus r) SafeStatus
safeGetSafeStatus cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.safe -# _.getSafeStatus

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenDeploy
--------------------------------------------------------------------------------
type TokenDeployOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrTokenDeploy r = ErrService + ErrNative + r

tokenDeploy :: forall r. B.CirclesCore -> B.Account -> TokenDeployOptions -> Result (ErrTokenDeploy r) String
tokenDeploy cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.token -# _.deploy

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenGetBalance
--------------------------------------------------------------------------------
type TokenGetBalanceOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrTokenGetBalance r = ErrNative + r

tokenGetBalance :: forall r. B.CirclesCore -> B.Account -> TokenGetBalanceOptions -> Result (ErrTokenGetBalance r) BN
tokenGetBalance cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.token -# _.getBalance

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenCheckUBIPayout
--------------------------------------------------------------------------------
type TokenCheckUBIPayoutOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrTokenCheckUBIPayout r = ErrNative + r

tokenCheckUBIPayout :: forall r. B.CirclesCore -> B.Account -> TokenCheckUBIPayoutOptions -> Result (ErrTokenCheckUBIPayout r) BN
tokenCheckUBIPayout cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.token -# _.checkUBIPayout

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenRequestUBIPayout
--------------------------------------------------------------------------------
type TokenRequestUBIPayoutOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrTokenRequestUBIPayout r = ErrNative + r

tokenRequestUBIPayout :: forall r. B.CirclesCore -> B.Account -> TokenRequestUBIPayoutOptions -> Result (ErrTokenRequestUBIPayout r) String
tokenRequestUBIPayout cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.token -# _.requestUBIPayout

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / tokenTransfer
--------------------------------------------------------------------------------
type TokenTransferOptions =
  { from :: ChecksumAddress
  , to :: ChecksumAddress
  , value :: BN
  , paymentNote :: String
  }

type ErrTokenTransfer r = ErrNative + ErrNullReturn + r

tokenTransfer :: forall r. B.CirclesCore -> B.Account -> TokenTransferOptions -> Result (ErrTokenTransfer r) String
tokenTransfer cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.token -# _.transfer

  mapArg2 x = x { from = show x.from, to = show x.to }

  mapOk x = case UU.toEither1 x of
    Left str -> pure str
    Right _ -> throwError $ inj (Proxy :: _ "errNullReturn") unit

--------------------------------------------------------------------------------
-- API / getPaymentNote
--------------------------------------------------------------------------------
type GetPaymentNoteOptions =
  { transactionHash :: String
  }

type ErrGetPaymentNote r = ErrNative + ErrNotGivenOrAllowed + r

tokenGetPaymentNote :: forall r. B.CirclesCore -> B.Account -> GetPaymentNoteOptions -> Result (ErrGetPaymentNote r) String
tokenGetPaymentNote cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.token -# _.getPaymentNote

  mapArg2 x = x { transactionHash = x.transactionHash }

  mapOk x = N.toMaybe x # note (inj (Proxy :: _ "errNotGivenOrAllowed") unit) # liftEither

--------------------------------------------------------------------------------
-- API / organizationIsOrganization
--------------------------------------------------------------------------------
type OrganizationIsOrganizationOptions =
  { safeAddress :: ChecksumAddress
  }

type ErrOrganizationIsOrganization r = ErrService + ErrNative + r

organizationIsOrganization :: forall r. B.CirclesCore -> B.Account -> OrganizationIsOrganizationOptions -> Result (ErrOrganizationIsOrganization r) Boolean
organizationIsOrganization cc = mapFn2 fn pure (mapArg2 >>> pure) mkErrorNative pure
  where
  fn = convertCore cc -# _.organization -# _.isOrganization

  mapArg2 x = x { safeAddress = show x.safeAddress }

--------------------------------------------------------------------------------
-- API / utilsRequestRelayer
--------------------------------------------------------------------------------

type UtilsRequestRelayerOptions =
  { path :: Array String
  , version :: Int
  , method :: String
  , data ::
      { saltNonce :: Nonce
      , owners :: Array ChecksumAddress
      , threshold :: Int
      }
  }

type ErrUtilsRequestRelayer r = ErrNative + ErrParseAddress + r

newtype SafeAddress = SafeAddress Address

derive newtype instance Show SafeAddress

derive newtype instance Eq SafeAddress

derive newtype instance Ord SafeAddress

derive newtype instance DecodeJson SafeAddress

derive newtype instance EncodeJson SafeAddress

derive newtype instance WriteForeign SafeAddress

derive newtype instance ReadForeign SafeAddress

utilsRequestRelayer :: forall r. B.CirclesCore -> UtilsRequestRelayerOptions -> Result (ErrUtilsRequestRelayer r) SafeAddress
utilsRequestRelayer cc = mapFn1 fn (mapArg1 >>> pure) mkErrorNative mapOk
  where
  fn = convertCore cc -# _.utils -# _.requestRelayer

  mapArg1 x = x
    { data =
        { saltNonce: unwrap x.data.saltNonce
        , owners: map show x.data.owners
        , threshold: x.data.threshold
        }
    }

  mapOk x = parseAddr x.safe <#> SafeAddress

--------------------------------------------------------------------------------
-- Err slices
--------------------------------------------------------------------------------
type ErrNative r = (errNative :: NativeError | r)

type ErrInvalidUrl r = (errInvalidUrl :: String | r)

type ErrApi r = (errApi :: ApiError | r)

type ErrNullReturn r = (errNullReturn :: Unit | r)

type ErrService r = (errService :: Unit | r)

type ErrParseAddress r = (errParseAddress :: String | r)

type ErrNotGivenOrAllowed r = (errNotGivenOrAllowed :: Unit | r)

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

_errParseAddress :: forall r. String -> Variant (ErrParseAddress r)
_errParseAddress = inj (Proxy :: _ "errParseAddress")

--------------------------------------------------------------------------------
-- Err composition
--------------------------------------------------------------------------------
type Err r = ErrParseAddress
  + ErrNative
  + ErrService
  + ErrInvalidUrl
  + ErrApi
  + ErrNotGivenOrAllowed
  + ErrNullReturn
  + r

printErr :: Variant (Err ()) -> String
printErr =
  case_
    # on (Proxy :: _ "errNative") (\e -> "Native: " <> e.name <> ": " <> e.message)
    # on (Proxy :: _ "errService") (\_ -> "service error")
    # on (Proxy :: _ "errInvalidUrl") (\url -> "Invalid URL: " <> url)
    # on (Proxy :: _ "errApi") (\e -> "Api Error: " <> show e)
    # on (Proxy :: _ "errParseAddress") (\e -> "ParseAddress error: " <> show e)
    # on (Proxy :: _ "errNullReturn") (\_ -> "Null return")
    # on (Proxy :: _ "errNotGivenOrAllowed") (\_ -> "Not given or allowed.")

--------------------------------------------------------------------------------
-- Error types
--------------------------------------------------------------------------------
type NativeError =
  { message :: String
  , name :: String
  }

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
userToUser :: forall r. B.User -> Either (Variant (ErrParseAddress r)) User
userToUser (B.User x) = do
  safeAddress <- parseAddr x.safeAddress
  pure $ x { safeAddress = safeAddress }

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

parseAddr :: forall r. String -> Either (Variant (ErrParseAddress r)) Address
parseAddr s = s
  # mkHexString
  >>= mkAddress
  # note (_errParseAddress s)
