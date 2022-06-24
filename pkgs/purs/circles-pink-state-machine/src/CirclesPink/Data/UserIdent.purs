module CirclesPink.Data.UserIdent
  ( UserIdent(..)
  , getAddress
  ) where

import CirclesCore (User)
import Convertable (convert)
import Data.Either (Either(..))
import Data.IxGraph (class Indexed)
import Network.Ethereum.Core.Signatures as W3
import Wallet.PrivateKey (Address)

newtype UserIdent = UserIdent (Either Address User)

instance indexedUserIdent :: Indexed W3.Address UserIdent where
  getIndex (UserIdent (Left x)) = convert x
  getIndex (UserIdent (Right { safeAddress })) = convert safeAddress

getAddress :: UserIdent -> Address
getAddress (UserIdent (Left addr)) = addr
getAddress (UserIdent (Right { safeAddress })) = safeAddress
