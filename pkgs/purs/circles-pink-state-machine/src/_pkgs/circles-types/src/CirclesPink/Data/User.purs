module CirclesPink.Data.User where

import Prelude

import CirclesPink.Data.Address (Address)
import Data.Newtype (class Newtype)
import PursTsGen (class ToTsDef, class ToTsType, PursType(..), defaultToPursType, opaqueToTsDef, typeRefToTsType)
import PursTsGen.Class.ToPursType (class ToPursType)

newtype User = User
  { id :: Int
  , username :: String
  , safeAddress :: Address
  , avatarUrl :: String
  }

derive instance Newtype User _

derive newtype instance Show User

derive newtype instance Eq User

derive newtype instance Ord User

ptUser :: PursType
ptUser = PursType "CirclesPink_Data_User" "User"

instance ToTsDef User where
  toTsDef _ = opaqueToTsDef ptUser []

instance ToTsType User where
  toTsType _ = typeRefToTsType ptUser []

instance ToPursType User where
  toPursType _ = defaultToPursType ptUser []
