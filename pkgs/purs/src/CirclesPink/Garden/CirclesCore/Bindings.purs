module CirclesPink.Garden.CirclesCore.Bindings
  ( CirclesCore
  , Options
  , Provider
  , UserOptions
  , Web3
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , userRegister
  ) where

import Effect (Effect)

foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

type Options
  = { apiServiceEndpoint :: String
    , graphNodeEndpoint :: String
    , hubAddress :: String
    , proxyFactoryAddress :: String
    , relayServiceEndpoint :: String
    , safeMasterAddress :: String
    , subgraphName :: String
    }

foreign import newCirclesCore :: Web3 -> Options -> Effect CirclesCore

type UserOptions
  = { nonce :: Int
    , safeAddress :: String
    , username :: String
    , email :: String
    }

foreign import userRegister :: CirclesCore -> UserOptions -> Effect Boolean
