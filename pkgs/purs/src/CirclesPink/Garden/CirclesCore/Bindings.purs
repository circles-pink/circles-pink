module CirclesPink.Garden.CirclesCore.Bindings
  ( Account
  , CirclesCore
  , Options
  , Provider
  , UserOptions
  , Web3
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , safePredictAddress
  , safePrepareDeploy
  , userRegister
  ) where

import Prelude
import Data.BigInt (BigInt)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import data Account :: Type

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

foreign import safePredictAddressImpl :: CirclesCore -> Account -> { nonce :: BigInt } -> EffectFnAff String

foreign import safePrepareDeployImpl :: CirclesCore -> Account -> { nonce :: BigInt } -> EffectFnAff String

safePredictAddress :: CirclesCore -> Account -> { nonce :: BigInt } -> Aff String
safePredictAddress x1 x2 x3 = fromEffectFnAff $ safePredictAddressImpl x1 x2 x3

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

foreign import userRegisterImpl :: CirclesCore -> Account -> UserOptions -> EffectFnAff Boolean

userRegister :: CirclesCore -> Account -> UserOptions -> Aff Boolean
userRegister x1 x2 x3 = fromEffectFnAff $ userRegisterImpl x1 x2 x3
