module Web3.Bindings
  ( Account
  , Eth
  , Provider
  , Utils
  , Web3
  , Web3Static
  , accountsHashMessage
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , sendTransaction
  , web3static
  )
  where

import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff)
import Simple.JSON (class ReadForeign)

foreign import web3static :: Web3Static

foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data Account :: Type

type Web3Static =
  { utils :: Utils
  , eth :: Eth
  }

newtype SignatureObj = SignatureObj
  { message :: String
  , messageHash :: String
  , v :: String
  , r :: String
  , s :: String
  , signature :: String
  }


derive instance newtypeSignatureObj :: Newtype SignatureObj _

derive newtype instance readForeign :: ReadForeign SignatureObj


foreign import sendTransaction :: Web3 -> { from :: String, to :: String, value :: String } -> EffectFnAff String

foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

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
