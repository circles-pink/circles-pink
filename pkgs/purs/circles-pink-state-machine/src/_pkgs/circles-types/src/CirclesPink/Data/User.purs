module CirclesPink.Data.User where

import Prelude

import CirclesPink.Data.Address (Address)
import Data.Newtype (class Newtype)

newtype User = User
  { id :: Int
  , username :: String
  , safeAddress :: Address
  , avatarUrl :: String
  }

derive instance newtypeUser :: Newtype User _

derive newtype instance showUser :: Show User

derive newtype instance eqUser :: Eq User

derive newtype instance ordUser :: Ord User


