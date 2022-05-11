module Web3.Bindings
  ( Account
  , Eth
  , Provider
  , Utils
  , Web3
  , Web3Static
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , sendTransaction
  , web3static
  ) where

import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff)

foreign import web3static :: Web3Static

foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data Account :: Type

type Web3Static
  = { utils :: Utils
    , eth :: Eth
    }

foreign import sendTransaction :: Web3 -> { from :: String, to :: String, value :: Number } -> EffectFnAff String

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

--------------------------------------------------------------------------------
type Utils
  = { toChecksumAddress :: String -> String
    }

type Eth
  = {
    }
