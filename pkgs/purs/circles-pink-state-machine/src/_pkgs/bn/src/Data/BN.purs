module Data.BN
  ( BN
  , fromDecimalStr
  , toDecimalStr
  ) where

import Prelude

foreign import data BN :: Type
foreign import fromStrImpl :: String -> Int -> BN
foreign import toStrImpl :: BN -> Int -> String

foreign import eqImpl :: BN -> BN -> Boolean

foreign import addImpl :: BN -> BN -> BN
foreign import mulImpl :: BN -> BN -> BN

fromDecimalStr :: String -> BN
fromDecimalStr s = fromStrImpl s 10

toDecimalStr :: BN -> String
toDecimalStr bn = toStrImpl bn 10

instance show :: Show BN where
  show = toDecimalStr

instance eq :: Eq BN where
  eq = eqImpl

instance semiring :: Semiring BN where
  add = addImpl
  mul = mulImpl
  zero = fromDecimalStr ("0")
  one = fromDecimalStr ("1")
