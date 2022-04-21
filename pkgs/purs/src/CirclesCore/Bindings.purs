module CirclesCore.Bindings
  ( Account
  , ApiError
  , ApiResult
  , CirclesCore
  , CirclesCore_
  , Options
  , Provider
  , ResolveOptions
  , TrustIsTrustedOptions
  , TrustIsTrustedResult
  , TrustNode
  , User
  , UserOptions
  , Web3
  , apiResultToEither
  , convertCore
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , safePredictAddress
  , safePrepareDeploy
  , safePrepareDeployImpl
  , trustGetNetwork
  , trustIsTrusted
  , unsafeSampleCore
  , userRegister
  , userResolve
  , userResolveImpl
  ) where

import Prelude
import Control.Promise (Promise)
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object)
import Foreign.Object.Unsafe (unsafeIndex)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import data Account :: Type

type User
  = { id :: Int
    , username :: String
    , safeAddress :: String
    , avatarUrl :: String
    }

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

foreign import safePredictAddress :: CirclesCore -> Account -> { nonce :: BigInt } -> EffectFnAff String

--------------------------------------------------------------------------------
-- FFI / safePrepareDeploy
--------------------------------------------------------------------------------
foreign import safePrepareDeployImpl :: CirclesCore -> Account -> { nonce :: BigInt } -> EffectFnAff String

safePrepareDeploy :: CirclesCore -> Account -> { nonce :: BigInt } -> Aff String
safePrepareDeploy x1 x2 x3 = fromEffectFnAff $ safePrepareDeployImpl x1 x2 x3

--------------------------------------------------------------------------------
-- FFI / newCirclesCore
--------------------------------------------------------------------------------
type Options
  = { apiServiceEndpoint :: String
    , graphNodeEndpoint :: String
    , hubAddress :: String
    , proxyFactoryAddress :: String
    , relayServiceEndpoint :: String
    , safeMasterAddress :: String
    , subgraphName :: String
    , databaseSource :: String
    }

foreign import newCirclesCore :: Web3 -> Options -> Effect CirclesCore

type CirclesCore_
  = { user ::
        { register ::
            Fn2 Account
              { nonce :: BigInt
              , safeAddress :: String
              , username :: String
              , email :: String
              }
              (Promise Boolean)
        , resolve ::
            Fn2 Account
              { addresses :: Array String
              , userNames :: Array String
              }
              (Promise (ApiResult (Array User)))
        }
    , safe ::
        { deploy ::
            Fn2 Account
              { safeAddress :: String }
              (Promise Boolean)
        , isFunded ::
            Fn2 Account
              { safeAddress :: String }
              (Promise Boolean)
        , getSafeStatus ::
            Fn2 Account
              { safeAddress :: String }
              ( Promise
                  { isCreated :: Boolean
                  , isDeployed :: Boolean
                  }
              )
        }
    , token ::
        { deploy ::
            Fn2 Account
              { safeAddress :: String }
              (Promise String)
        }
    }

foreign import mkCirclesCore :: Web3 -> Options -> Effect CirclesCore_

--------------------------------------------------------------------------------
-- FFI / userRegister
--------------------------------------------------------------------------------
type UserOptions
  = { nonce :: BigInt
    , safeAddress :: String
    , username :: String
    , email :: String
    }

foreign import userRegisterImpl :: CirclesCore -> Account -> UserOptions -> EffectFnAff Boolean

userRegister :: CirclesCore -> Account -> UserOptions -> Aff Boolean
userRegister x1 x2 x3 = fromEffectFnAff $ userRegisterImpl x1 x2 x3

--------------------------------------------------------------------------------
-- userResolve
--------------------------------------------------------------------------------
type ResolveOptions
  = { addresses :: Array String
    , userNames :: Array String
    }

foreign import userResolveImpl :: CirclesCore -> Account -> ResolveOptions -> EffectFnAff (ApiResult (Array User))

userResolve :: CirclesCore -> Account -> ResolveOptions -> Aff (ApiResult (Array User))
userResolve x1 x2 x3 = fromEffectFnAff $ userResolveImpl x1 x2 x3

--------------------------------------------------------------------------------
-- trustGetNetwork
--------------------------------------------------------------------------------
type TrustNode
  = { isIncoming :: Boolean
    , isOutgoing :: Boolean
    , limitPercentageIn :: Int
    , limitPercentageOut :: Int
    , mutualConnections :: Array Foreign
    , safeAddress :: String
    }

foreign import trustGetNetwork :: CirclesCore -> Account -> { safeAddress :: String } -> EffectFnAff (Array TrustNode)

--------------------------------------------------------------------------------
-- trustIsTrusted
--------------------------------------------------------------------------------
type TrustIsTrustedOptions
  = { safeAddress :: String
    , limit :: Int
    }

type TrustIsTrustedResult
  = { trustConnections :: Int
    , isTrusted :: Boolean
    }

foreign import trustIsTrusted :: CirclesCore -> Account -> TrustIsTrustedOptions -> EffectFnAff TrustIsTrustedResult

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type ApiError
  = { message :: String, code :: Int }

newtype ApiResult :: forall k. k -> Type
newtype ApiResult a
  = ApiResult (Object Foreign)

apiResultToEither :: forall a. ApiResult a -> Either ApiError a
apiResultToEither (ApiResult fo) =
  let
    status = unsafeIndex fo "status" # unsafeFromForeign
  in
    if status == "ok" then
      let
        data_ = unsafeIndex fo "data" # unsafeFromForeign
      in
        Right data_
    else
      let
        code = unsafeIndex fo "code" # unsafeFromForeign

        message = unsafeIndex fo "message" # unsafeFromForeign
      in
        Left { code, message }

foreign import unsafeSampleCore :: CirclesCore -> Account -> EffectFnAff Unit

convertCore :: CirclesCore -> CirclesCore_
convertCore = unsafeCoerce
