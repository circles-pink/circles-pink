module Web3.Accounts
  ( Hash(..)
  , SignatureObj(..)
  , hashMessage
  , recover
  , sign
  )
  where

import Data.Either (Either)
import Data.Newtype (class Newtype)
import Debug.Extra (todo)
import Effect.Exception (Error)
import Network.Ethereum.Core.Signatures (Address, PrivateKey)
import Simple.JSON (class ReadForeign)

newtype Hash = Hash String

derive instance newtypeHash :: Newtype Hash _

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

sign :: String -> PrivateKey -> SignatureObj
sign = todo

recover :: SignatureObj -> Either Error Address
recover = todo

hashMessage :: String -> Hash
hashMessage = todo