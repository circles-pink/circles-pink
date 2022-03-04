module CirclesPink.Garden.CirclesCore
  ( CirclesCore
  , Options
  , Provider
  , Web3
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  ) where

import Data.Either (Either(..))
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
newCirclesCore :: Web3 -> Options -> Effect (Either String CirclesCore)
newCirclesCore = _newCirclesCore mkEither

foreign import _newCirclesCore :: MkEither -> Web3 -> Options -> Effect (Either String CirclesCore)

foreign import newWeb3 :: Provider -> Effect Web3

newWebSocketProvider :: String -> Effect (Either String Provider)
newWebSocketProvider = _newWebSocketProvider mkEither

type MkEither
  = { left :: forall a b. a -> Either a b
    , right :: forall a b. b -> Either a b
    }

mkEither :: MkEither
mkEither = { left: Left, right: Right }

foreign import _newWebSocketProvider :: MkEither -> String -> Effect (Either String Provider)

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import data Provider :: Type
