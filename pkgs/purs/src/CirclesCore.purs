module CirclesCore
  ( Err
  , ErrInvalidUrl
  , ErrNative
  , ErrService
  , NativeError
  , SafeStatus
  , TrustNode
  , User
  , UserOptions
  , _errInvalidUrl
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
  , tokenDeploy
  , trustGetNetwork
  , trustIsTrusted
  , unsafeSampleCore
  , userRegister
  , userResolve
  ) where

import Prelude
import CirclesCore.Bindings (ApiError, apiResultToEither, convertCore)
import CirclesCore.Bindings (Options, Provider, Web3, CirclesCore, Account, ApiError, TrustIsTrustedResult) as Exp
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

newWebSocketProvider :: forall r. String -> ExceptV (ErrNative + ErrInvalidUrl + r) Effect B.Provider
newWebSocketProvider x1 =
  B.newWebSocketProvider x1
    # try
    <#> lmap evalError
    # ExceptT
  where
  evalError e = case name e, message e of
    "TypeError", "Invalid URL" -> inj (Proxy :: _ "errInvalidUrl") x1
    _, _ -> mkErrorNative e

newWeb3 :: B.Provider -> Effect B.Web3
newWeb3 = B.newWeb3

newCirclesCore :: forall r. B.Web3 -> B.Options -> ExceptV (ErrNative + r) Effect B.CirclesCore
newCirclesCore x1 x2 =
  B.newCirclesCore x1 x2
    # try
    <#> lmap mkErrorNative
    # ExceptT

privKeyToAccount :: forall r. B.Web3 -> PrivateKey -> ExceptV (ErrNative + r) Effect B.Account
privKeyToAccount w3 pk =
  B.privKeyToAccount w3 (P.toString pk)
    # try
    <#> lmap mkErrorNative
    # ExceptT

safePredictAddress :: forall r. B.CirclesCore -> B.Account -> { nonce :: P.Nonce } -> Result (ErrNative + r) P.Address
safePredictAddress cc ac opts =
  B.safePredictAddress cc ac { nonce: P.nonceToBigInt opts.nonce }
    # fromEffectFnAff
    <#> P.unsafeAddrFromString
    # attempt
    <#> lmap mkErrorNative
    # ExceptT

safePrepareDeploy :: forall r. B.CirclesCore -> B.Account -> { nonce :: P.Nonce } -> Result (ErrNative + r) P.Address
safePrepareDeploy cc ac opts =
  B.safePrepareDeploy cc ac { nonce: P.nonceToBigInt opts.nonce }
    <#> P.unsafeAddrFromString
    # attempt
    <#> lmap mkErrorNative
    # ExceptT

--------------------------------------------------------------------------------
-- API / trustIsTrusted
--------------------------------------------------------------------------------
type TrustIsTrustedOptions
  = { safeAddress :: Address
    , limit :: Int
    }

trustIsTrusted :: forall r. B.CirclesCore -> B.Account -> TrustIsTrustedOptions -> Result (ErrNative + r) B.TrustIsTrustedResult
trustIsTrusted cc = mapFn2 (convertCore cc).trust.isTrusted pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x =
    x
      { safeAddress = addrToString x.safeAddress
      }

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

trustGetNetwork :: forall r. B.CirclesCore -> B.Account -> { safeAddress :: P.Address } -> Result (ErrNative + r) (Array TrustNode)
trustGetNetwork cc ac opts =
  B.trustGetNetwork cc ac { safeAddress: P.addrToString opts.safeAddress }
    # fromEffectFnAff
    <#> map conformTrustNode
    # attempt
    <#> lmap mkErrorNative
    # ExceptT
  where
  conformTrustNode :: B.TrustNode -> TrustNode
  conformTrustNode tn = tn { safeAddress = P.unsafeAddrFromString tn.safeAddress }

--------------------------------------------------------------------------------
-- API / userRegister
--------------------------------------------------------------------------------
type UserOptions
  = { nonce :: Nonce
    , safeAddress :: Address
    , username :: String
    , email :: String
    }

userRegister :: forall r. B.CirclesCore -> B.Account -> UserOptions -> Result (ErrService + ErrNative + r) Unit
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

userResolve :: forall r. B.CirclesCore -> B.Account -> ResolveOptions -> Result (ErrNative + ErrApi + r) (Array User)
userResolve cc ac opts =
  B.userResolve cc ac
    { addresses: map addrToString opts.addresses
    , userNames: opts.userNames
    }
    # attempt
    <#> lmap mkErrorNative
    # map (\x -> x >>= handleApiResult)
    # ExceptT
  where
  handleApiResult apiResult = case apiResultToEither apiResult of
    Left apiError -> Left $ inj (Proxy :: _ "errApi") apiError
    Right data_ -> pure $ map userToUser data_

  userToUser u =
    { id: u.id
    , username: u.username
    , safeAddress: P.unsafeAddrFromString u.safeAddress
    , avatarUrl: u.avatarUrl
    }

--------------------------------------------------------------------------------
-- API / safeDeploy
--------------------------------------------------------------------------------
type SafeDeployOptions
  = { safeAddress :: Address
    }

safeDeploy :: forall r. B.CirclesCore -> B.Account -> SafeDeployOptions -> Result (ErrService + ErrNative + r) Unit
safeDeploy cc = mapFn2 (convertCore cc).safe.deploy pure (mapArg2 >>> pure) mkErrorNative mapBoolean
  where
  mapArg2 x =
    x
      { safeAddress = addrToString x.safeAddress
      }

--------------------------------------------------------------------------------
-- API / safeIsFunded
--------------------------------------------------------------------------------
type SafeIsFundedOptions
  = { safeAddress :: Address
    }

safeIsFunded :: forall r. B.CirclesCore -> B.Account -> SafeIsFundedOptions -> Result (ErrNative + r) Boolean
safeIsFunded cc = mapFn2 (convertCore cc).safe.isFunded pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x =
    x
      { safeAddress = addrToString x.safeAddress
      }

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

safeGetSafeStatus :: forall r. B.CirclesCore -> B.Account -> SafeGetSafeStatusOptions -> Result (ErrNative + r) SafeStatus
safeGetSafeStatus cc = mapFn2 (convertCore cc).safe.getSafeStatus pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x =
    x
      { safeAddress = addrToString x.safeAddress
      }

--------------------------------------------------------------------------------
-- API / tokenDeploy
--------------------------------------------------------------------------------
type TokenDeployOptions
  = { safeAddress :: Address
    }

tokenDeploy :: forall r. B.CirclesCore -> B.Account -> TokenDeployOptions -> Result (ErrService + ErrNative + r) String
tokenDeploy cc = mapFn2 (convertCore cc).token.deploy pure (mapArg2 >>> pure) mkErrorNative pure
  where
  mapArg2 x =
    x
      { safeAddress = addrToString x.safeAddress
      }

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------
type ErrNative r
  = ( errNative :: NativeError | r )

type ErrInvalidUrl r
  = ( errInvalidUrl :: String | r )

_errInvalidUrl :: forall r. String -> Variant (ErrInvalidUrl r)
_errInvalidUrl = inj (Proxy :: _ "errInvalidUrl")

type ErrApi r
  = ( errApi :: ApiError | r )

type ErrService r
  = ( errService :: Unit | r )

type Err r
  = ErrNative + ErrService + ErrInvalidUrl + r

type NativeError
  = { message :: String
    , name :: String
    }

printErr :: Variant (Err ()) -> String
printErr =
  case_
    # on (Proxy :: _ "errNative") (\e -> "Native: " <> e.name <> ": " <> e.message)
    # on (Proxy :: _ "errService") (\_ -> "service error")
    # on (Proxy :: _ "errInvalidUrl") (\url -> "Invalid URL: " <> url)

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
mkErrorNative e = inj (Proxy :: _ "errNative") { message: message e, name: name e }

mapBoolean :: forall r. Boolean -> Either (Variant (ErrService + r)) Unit
mapBoolean true = Right unit

mapBoolean false = Left $ inj (Proxy :: _ "errService") unit
