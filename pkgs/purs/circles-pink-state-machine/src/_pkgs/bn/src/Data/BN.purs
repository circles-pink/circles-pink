module Data.BN
  ( BN
  , fromDecimalStr
  , toDecimalStr
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Either (note)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Regex (Regex)
import Data.String.Regex as Reg
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

foreign import data BN :: Type
foreign import fromStrImpl :: String -> Int -> BN
foreign import toStrImpl :: BN -> Int -> String

foreign import eqImpl :: BN -> BN -> Boolean

foreign import addImpl :: BN -> BN -> BN
foreign import mulImpl :: BN -> BN -> BN

fromDecimalStr :: String -> Maybe BN
fromDecimalStr s | Reg.test regex s = pure $ fromStrImpl s 10
fromDecimalStr _ = Nothing

regex :: Regex
regex = unsafeRegex "^[0-9]+$" noFlags

toDecimalStr :: BN -> String
toDecimalStr bn = toStrImpl bn 10

instance Show BN where
  show = toDecimalStr

instance Eq BN where
  eq = eqImpl

instance Semiring BN where
  add = addImpl
  mul = mulImpl
  zero = unsafePartial $ fromJust $ fromDecimalStr ("0")
  one = unsafePartial $ fromJust $ fromDecimalStr ("1")

instance DecodeJson BN where
  decodeJson x = decodeJson x >>= fromDecimalStr >>> note (TypeMismatch "Cannot parse BN")