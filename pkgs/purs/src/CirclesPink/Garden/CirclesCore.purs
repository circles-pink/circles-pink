module CirclesPink.Garden.CirclesCore where

import Data.Either (Either)
import Effect (Effect)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type Options
  = { apiServiceEndpoint :: String
    , graphNodeEndpoint :: String
    , hubAddress :: String
    , proxyFactoryAddress :: String
    , relayServiceEndpoint :: String
    , safeMasterAddress :: String
    , subgraphName :: String
    }

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import newCirclesCore :: Web3 -> Options -> Effect CirclesCore

foreign import newWeb3 :: Provider -> Effect (Either String Web3)

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import data Provider :: Type
