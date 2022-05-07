module FFI where

import Prelude
import Prim.RowList (class RowToList, Cons, Nil)

class PrimData :: forall k. k -> Constraint
class PrimData a

instance primDataInt :: PrimData Int

instance primDataString :: PrimData String

instance primDataChar :: PrimData Char

instance primDataBoolean :: PrimData Boolean

instance primDataNumber :: PrimData Number

instance primDataArray :: PrimData a => PrimData (Array (a))

instance primDataRecord :: (RowToList r rl, PrimDataRecord rl) => PrimData (Record r)

class PrimDataRecord :: forall k. k -> Constraint
class PrimDataRecord rl

instance primDataRecordNil :: PrimDataRecord Nil

instance primDataRecordCons :: PrimDataRecord rl => PrimDataRecord (Cons s t rl)

--------------------------------------------------------------------------------
checkPrimData :: forall a. PrimData a => a -> Unit
checkPrimData _ = unit

--------------------------------------------------------------------------------
checked :: Unit
checked = checkPrimData {}
