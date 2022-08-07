module CirclesPink.Data.User where

import Prelude

import CirclesPink.Data.Address (Address)
import Data.Newtype (class Newtype)
import PursTsGen (class ToTsDef, class ToTsType, PursType(..), defaultToPursType, defaultToTsDef, defaultToTsType)
import PursTsGen.Class.ToPursType (class ToPursType)

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

ptUser :: PursType
ptUser = PursType "CirclesPink_Data_User" "User"

instance toTsTypeDefUser :: ToTsDef User where
  toTsDef _ = defaultToTsDef ptUser []

instance toTsTypeUser :: ToTsType User where
  toTsType _ = defaultToTsType ptUser []

instance toPursTypeUser :: ToPursType User where
  toPursType _ = defaultToPursType ptUser []
