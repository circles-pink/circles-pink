module FpTs.Class where

import Prelude

class FpTs a b where
  toFpTs ::  a -> b
  fromFpTs :: b -> a

instance fpTsInt :: FpTs Int Int where
  toFpTs = identity
  fromFpTs = identity

instance fpTsNumber :: FpTs Number Number where
  toFpTs = identity
  fromFpTs = identity

instance fpTsBoolean :: FpTs Boolean Boolean where
  toFpTs = identity
  fromFpTs = identity

instance fpTsString :: FpTs String String where
  toFpTs = identity
  fromFpTs = identity

instance fpTsFunction :: FpTs (Function a b) (Function a b) where
  toFpTs = identity
  fromFpTs = identity

instance fpTsArray :: FpTs a b => FpTs (Array a) (Array b) where
  toFpTs = map toFpTs 
  fromFpTs = map fromFpTs

