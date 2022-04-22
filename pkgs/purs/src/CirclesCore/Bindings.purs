module CirclesCore.Bindings
  ( Account
  , CirclesCore
  , CirclesCore_
  , Fn2Promise
  , Fn3Promise
  , Options
  , Provider
  , TrustIsTrustedResult
  , User
  , Web3
  , convertCore
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , unsafeSampleCore
  ) where

import Prelude
import CirclesCore.ApiResult (ApiResult)
import Control.Promise (Promise)
import Data.BigInt (BigInt)
import Data.Function.Uncurried (Fn2, Fn3)
import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff)
import Foreign (Foreign)
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

type TrustIsTrustedResult
  = { trustConnections :: Int
    , isTrusted :: Boolean
    }

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

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
            Fn2Promise Account
              { nonce :: BigInt
              , safeAddress :: String
              , username :: String
              , email :: String
              }
              Boolean
        , resolve ::
            Fn2Promise Account
              { addresses :: Array String
              , userNames :: Array String
              }
              (ApiResult (Array User))
        }
    , safe ::
        { deploy :: Fn2Promise Account { safeAddress :: String } Boolean
        , isFunded :: Fn2Promise Account { safeAddress :: String } Boolean
        , predictAddress :: Fn2Promise Account { nonce :: BigInt } String
        , prepareDeploy :: Fn2Promise Account { nonce :: BigInt } String
        , getSafeStatus ::
            Fn2Promise Account { safeAddress :: String }
              { isCreated :: Boolean
              , isDeployed :: Boolean
              }
        }
    , token ::
        { deploy :: Fn2Promise Account { safeAddress :: String } String
        }
    , trust ::
        { isTrusted ::
            Fn2Promise Account
              { safeAddress :: String
              , limit :: Int
              }
              TrustIsTrustedResult
        , getNetwork ::
            Fn2Promise Account { safeAddress :: String }
              ( Array
                  { isIncoming :: Boolean
                  , isOutgoing :: Boolean
                  , limitPercentageIn :: Int
                  , limitPercentageOut :: Int
                  , mutualConnections :: Array Foreign
                  , safeAddress :: String
                  }
              )
        }
    }

foreign import mkCirclesCore :: Web3 -> Options -> Effect CirclesCore_

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
foreign import unsafeSampleCore :: CirclesCore -> Account -> EffectFnAff Unit

convertCore :: CirclesCore -> CirclesCore_
convertCore = unsafeCoerce

type Fn2Promise a1 a2 b
  = Fn2 a1 a2 (Promise b)

type Fn3Promise a1 a2 a3 b
  = Fn3 a1 a2 a3 (Promise b)
