module FpTs.Class where

import Prelude

import Heterogeneous.Mapping (class HMap, class Mapping, hmap)

class FpTs a b where 
  toFpTs :: a -> b
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

--------------------------------------------------------------------------------

data FromFpTs = FromFpTs

instance fromFpTsInst ::
  ( FpTs b a
  ) =>
  Mapping FromFpTs a b where
  mapping FromFpTs = fromFpTs

data ToFpTs = ToFpTs

instance toFpTsInst ::
  ( FpTs a b
  ) =>
  Mapping ToFpTs a b where
  mapping ToFpTs = toFpTs

instance fpTsRecord ::
  ( HMap ToFpTs (Record r) (Record r')
  , HMap FromFpTs (Record r') (Record r)
  ) =>
  FpTs (Record r) (Record r') where
  toFpTs = hmap ToFpTs
  fromFpTs = hmap FromFpTs
