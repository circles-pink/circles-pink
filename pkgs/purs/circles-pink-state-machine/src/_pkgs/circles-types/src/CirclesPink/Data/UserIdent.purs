module CirclesPink.Data.UserIdent
  ( UserIdent(..)
  , getAddress
  , getIdentifier
  ) where

import CirclesPink.Prelude

import CirclesCore (User)
import CirclesPink.Data.Address (Address)
import Data.IxGraph (class Indexed)
import Data.String as S
import FpTs.Class (class FpTs)

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

shortenAddrBy ∷ Int
shortenAddrBy = 6

getIdentifier :: UserIdent -> String
getIdentifier (UserIdent (Left addr)) = S.take shortenAddrBy $ show addr
getIdentifier (UserIdent (Right { username })) = username