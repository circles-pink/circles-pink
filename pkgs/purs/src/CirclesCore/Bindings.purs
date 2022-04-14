module CirclesCore.Bindings
  ( Account
  , ApiError
  , ApiResult
  , CirclesCore
  , Options
  , Provider
  , ResolveOptions
  , User
  , UserOptions
  , Web3
  , apiResultToEither
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , safePredictAddress
  , safePrepareDeploy
  , userRegister
  , userResolve
  , userResolveImpl
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object)
import Foreign.Object.Unsafe (unsafeIndex)

foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import data Account :: Type

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

foreign import safePredictAddress :: CirclesCore -> Account -> { nonce :: BigInt } -> EffectFnAff String

foreign import safePrepareDeployImpl :: CirclesCore -> Account -> { nonce :: BigInt } -> EffectFnAff String

safePrepareDeploy :: CirclesCore -> Account -> { nonce :: BigInt } -> Aff String
safePrepareDeploy x1 x2 x3 = fromEffectFnAff $ safePrepareDeployImpl x1 x2 x3

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

type UserOptions
  = { nonce :: BigInt
    , safeAddress :: String
    , username :: String
    , email :: String
    }

--------------------------------------------------------------------------------
-- userRegister
--------------------------------------------------------------------------------
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

type User
  = { id :: Int
    , username :: String
    , safeAddress :: String
    , avatarUrl :: String
    }

newtype ApiResult a
  = ApiResult (Object Foreign)

type ApiError
  = { message :: String, code :: Int }

foreign import userResolveImpl :: CirclesCore -> Account -> ResolveOptions -> EffectFnAff (ApiResult (Array User))

userResolve :: CirclesCore -> Account -> ResolveOptions -> Aff (ApiResult (Array User))
userResolve x1 x2 x3 = fromEffectFnAff $ userResolveImpl x1 x2 x3

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
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
