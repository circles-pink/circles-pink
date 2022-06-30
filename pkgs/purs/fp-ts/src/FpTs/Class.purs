module FpTs.Class where

import Prelude

import Debug.Extra (todo)
import Prim.RowList (class RowToList, RowList)
import Type.Proxy (Proxy(..))

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

instance fpTsRecord ::
  ( RowToList r rl
  , GenericRecordToFpTs rl r'
  , RowToList r' rl'
  , GenericRecordFromFpTs rl' r
  ) =>
  FpTs (Record r) (Record r') where
  toFpTs _ = genRecordToFpTs (Proxy :: _ rl)
  fromFpTs _ = genRecordFromFpTs (Proxy :: _ rl')

class GenericRecordToFpTs :: RowList Type -> Row Type -> Constraint
class GenericRecordToFpTs rl r where
  genRecordToFpTs :: (Proxy rl) -> Record r

-- instance genericRecordToFpTsNil :: FpTs (Nil r) ->

class GenericRecordFromFpTs :: RowList Type -> Row Type -> Constraint
class GenericRecordFromFpTs rl r where
  genRecordFromFpTs :: (Proxy rl) -> Record r