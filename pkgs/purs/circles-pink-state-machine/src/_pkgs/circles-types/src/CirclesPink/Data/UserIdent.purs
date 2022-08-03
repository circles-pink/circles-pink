module CirclesPink.Data.UserIdent
  ( UserIdent'
  , UserIdent(..)
  , fromUser
  , getAddress
  , getIdentifier
  , unwrap
  )
  where

import CirclesPink.Prelude hiding (unwrap)

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.User (User(..))
import Data.IxGraph (class Indexed)
import Data.String as S
import FpTs.Class (class FpTs)
import PursTsGen (class ToTsDef, class ToTsType)
import PursTsGen.Lang.TypeScript.DSL as TS
import Data.Newtype as NT

type UserIdent' = Either Address User
newtype UserIdent = UserIdent UserIdent'

derive instance newtypeUserIdent :: Newtype UserIdent _
derive newtype instance showUserIdent :: Show UserIdent
derive newtype instance eqUserIdent :: Eq UserIdent
derive newtype instance ordUserIdent :: Ord UserIdent

instance toTsTypeDefUserIdent :: ToTsDef UserIdent where
  toTsDef _ = pure $ TS.typeDef (TS.name "UserIdent") []
    $ TS.opaque (TS.qualName "CirclesPink_Data_UserIdent" "UserIdent")
    $ TS.name
    <$> []

instance toTsTypeUserIdent :: ToTsType UserIdent where
  toTsType _ = TS.mkType_ $ TS.qualName "CirclesPink_Data_UserIdent" "UserIdent"

instance indexedUserIdent :: Indexed Address UserIdent where
  getIndex (UserIdent (Left x)) = x
  getIndex (UserIdent (Right (User { safeAddress }))) = safeAddress

instance ftTsUserIdent :: FpTs UserIdent UserIdent where
  fromFpTs = identity
  toFpTs = identity

unwrap :: UserIdent -> UserIdent'
unwrap = NT.unwrap

getAddress :: UserIdent -> Address
getAddress (UserIdent (Left addr)) = addr
getAddress (UserIdent (Right (User { safeAddress }))) = safeAddress

shortenAddrBy ∷ Int
shortenAddrBy = 6

getIdentifier :: UserIdent -> String
getIdentifier (UserIdent (Left addr)) = S.take shortenAddrBy $ show addr
getIdentifier (UserIdent (Right { username })) = username

fromUser :: User -> UserIdent
fromUser u = UserIdent $ Right u 
