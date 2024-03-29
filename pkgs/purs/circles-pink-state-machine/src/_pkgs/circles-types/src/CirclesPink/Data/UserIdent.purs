module CirclesPink.Data.UserIdent
  ( UserIdent'
  , UserIdent(..)
  , fromUser
  , getAddress
  , getIdentifier
  , unwrap
  ) where

import CirclesPink.Prelude hiding (unwrap)

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.User (User(..))
import Data.IxGraph (class Indexed)
import Data.Newtype as NT
import Data.String as S

type UserIdent' = Either Address User
newtype UserIdent = UserIdent UserIdent'

derive instance Newtype UserIdent _
derive newtype instance Show UserIdent
derive newtype instance Eq UserIdent
derive newtype instance Ord UserIdent

ptUserIdent :: PursType
ptUserIdent = PursType "CirclesPink_Data_UserIdent" "UserIdent"

instance ToTsDef UserIdent where
  toTsDef _ = opaqueToTsDef ptUserIdent []

instance ToTsType UserIdent where
  toTsType _ = typeRefToTsType ptUserIdent []

instance ToPursType UserIdent where
  toPursType _ = defaultToPursType ptUserIdent []

instance Indexed Address UserIdent where
  getIndex (UserIdent (Left x)) = x
  getIndex (UserIdent (Right (User { safeAddress }))) = safeAddress

unwrap :: UserIdent -> UserIdent'
unwrap = NT.unwrap

getAddress :: UserIdent -> Address
getAddress (UserIdent (Left addr)) = addr
getAddress (UserIdent (Right (User { safeAddress }))) = safeAddress

shortenAddrBy ∷ Int
shortenAddrBy = 6

getIdentifier :: UserIdent -> String
getIdentifier (UserIdent (Left addr)) = S.take shortenAddrBy $ show addr
getIdentifier (UserIdent (Right (User { username }))) = username

fromUser :: User -> UserIdent
fromUser u = UserIdent $ Right u
