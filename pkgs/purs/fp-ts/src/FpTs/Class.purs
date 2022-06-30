module FpTs.Class where

import Prelude

import Data.Symbol (class IsSymbol)
import Debug.Extra (todo)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))
import Record as R

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

-- instance fpTsRecord ::
--   ( RowToList r rl
--   , GenericRecordToFpTs rl () r'
--   , RowToList r' rl'
--   , GenericRecordFromFpTs rl' r
--   ) =>
--   FpTs (Record r) (Record r') where
--   toFpTs _ = genRecordToFpTs (Proxy :: _ rl) {}
--   fromFpTs _ = genRecordFromFpTs (Proxy :: _ rl')
-- --------------------------------------------------------------------------------

-- class GenericRecordToFpTs :: RowList Type -> Row Type -> Row Type -> Constraint
-- class GenericRecordToFpTs rl r1 r2 | rl -> r1 r2 where
--   genRecordToFpTs :: (Proxy rl) -> Record r1 -> Record r2

-- instance genericRecordToFpTsNil :: GenericRecordToFpTs Nil () () where
--   genRecordToFpTs _ r = r 

-- instance genericRecordToFpTsCons ::
--   ( IsSymbol s
--   , GenericRecordToFpTs rl r1 r2
--   --, Cons s a r' r
--   ) =>
--   GenericRecordToFpTs (Cons s a rl) r1 r2 where
--   genRecordToFpTs _ r = todo --R.set (Proxy :: _ s) 

-- --------------------------------------------------------------------------------

-- class GenericRecordFromFpTs :: RowList Type -> Row Type -> Constraint
-- class GenericRecordFromFpTs rl r where
--   genRecordFromFpTs :: (Proxy rl) -> Record r