module CirclesPink.Data.UserIdent
  ( UserIdent(..)
  , getAddress
  ) where

import Prelude

import CirclesCore (User)
import Data.Either (Either(..))
import Data.IxGraph (class Indexed)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Core.Signatures as W3

newtype UserIdent = UserIdent (Either Address User)

derive newtype instance showUserIdent :: Show UserIdent

derive newtype instance eqUserIdent :: Eq UserIdent

derive newtype instance ordUserIdent :: Ord UserIdent

instance indexedUserIdent :: Indexed W3.Address UserIdent where
  getIndex (UserIdent (Left x)) = x
  getIndex (UserIdent (Right { safeAddress })) = safeAddress

getAddress :: UserIdent -> Address
getAddress (UserIdent (Left addr)) = addr
getAddress (UserIdent (Right { safeAddress })) = safeAddress
