module CirclesPink.Data.UserIdent
  ( UserIdent(..)
  , getAddress
  ) where

import Prelude

import CirclesCore (User)
import Data.Either (Either(..))
import Data.IxGraph (class Indexed)
import FpTs.Class (class FpTs)
import CirclesPink.Data.Address (Address)

newtype UserIdent = UserIdent (Either Address User)

derive newtype instance showUserIdent :: Show UserIdent

derive newtype instance eqUserIdent :: Eq UserIdent

derive newtype instance ordUserIdent :: Ord UserIdent

instance indexedUserIdent :: Indexed Address UserIdent where
  getIndex (UserIdent (Left x)) = x
  getIndex (UserIdent (Right { safeAddress })) = safeAddress

instance ftTsUserIdent :: FpTs UserIdent UserIdent where
  fromFpTs = identity
  toFpTs = identity

getAddress :: UserIdent -> Address
getAddress (UserIdent (Left addr)) = addr
getAddress (UserIdent (Right { safeAddress })) = safeAddress
