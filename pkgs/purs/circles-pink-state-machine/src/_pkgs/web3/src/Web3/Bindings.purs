module Web3.Bindings
  ( Account
  , Eth
  , Provider
  , SignatureObj(..)
  , Utils
  , Web3
  , Web3Static
  , accountsHashMessage
  , accountsRecover
  , accountsSign
  , newWeb3
  , newWeb3_
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

type Web3Static =
  { utils :: Utils
  , eth :: Eth
  }

type SignatureObj =
  { message :: String
  , messageHash :: String
  , v :: String
  , r :: String
  , s :: String
  , signature :: String
  }

foreign import sendTransaction :: Web3 -> { from :: String, to :: String, value :: String } -> EffectFnAff String

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import newWeb3_ :: Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

foreign import accountsSign :: Web3 -> String -> String -> SignatureObj

foreign import accountsRecover :: Web3 -> SignatureObj -> Effect String

foreign import accountsHashMessage :: Web3 -> String -> String

--------------------------------------------------------------------------------
type Utils =
  { toChecksumAddress :: String -> String
  }

type Eth =
  {
  }
