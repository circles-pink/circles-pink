module PursTsGen.Class.ToTsType where

import Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import PursTsGen.Data.ABC (A, B, C, D, E, Z)
import PursTsGen.Lang.TypeScript.DSL ((|||))
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))

class ToTsType a where
  toTsType :: a -> TS.Type

instance toTsTypeNumber :: ToTsType Number where
  toTsType _ = TS.number

instance toTsTypeInt :: ToTsType Int where
  toTsType _ = TS.mkType_ $ TS.qualName "Data_Int" "Int"

instance toTsTypeString :: ToTsType String where
  toTsType _ = TS.string

instance toTsTypeBoolean :: ToTsType Boolean where
  toTsType _ = TS.boolean

instance toTsTypeArray :: ToTsType a => ToTsType (Array a) where
  toTsType _ = TS.array $ toTsType (undefined :: a)

instance toTsTypeRecord :: (RowToList r rl, GenRecord rl) => ToTsType (Record r) where
  toTsType _ = TS.record $ genRecord (Proxy :: _ rl)

instance toTsTypeFunction :: (ToTsType a, ToTsType b) => ToTsType (a -> b) where
  toTsType _ = TS.function_
    [ TS.keyVal "_" $ toTsType (undefined :: a) ]
    (toTsType (undefined :: b))

instance toTsTypeProxy :: ToTsType a => ToTsType (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

instance toTsTypeVariant :: (RowToList r rl, GenVariant rl) => ToTsType (Variant r) where
  toTsType _ = genVariant (Proxy :: _ rl)

instance toTsTypeMaybe :: ToTsType a => ToTsType (Maybe a) where
  toTsType _ = TS.mkType (TS.qualName "Data_Maybe" "Maybe")
    [ toTsType (Proxy :: _ a) ]

instance toTsTypeEither :: (ToTsType a, ToTsType b) => ToTsType (Either a b) where
  toTsType _ = TS.mkType (TS.qualName "Data_Either" "Either")
    [ toTsType (Proxy :: _ a), toTsType (Proxy :: _ b) ]

instance toTsTypeTuple :: (ToTsType a, ToTsType b) => ToTsType (Tuple a b) where
  toTsType _ = TS.mkType (TS.qualName "Data_Tuple" "Tuple")
    [ toTsType (Proxy :: _ a), toTsType (Proxy :: _ b) ]

instance toTsTypeUnit :: ToTsType Unit where
  toTsType _ = TS.mkType_ $ TS.qualName "Data_Unit" "Unit"

instance toTsTypeNullable :: ToTsType a => ToTsType (Nullable a) where
  toTsType _ = TS.null ||| toTsType (Proxy :: _ a)

instance toTsTypeA :: ToTsType A where
  toTsType _ = TS.var $ TS.name "A"

instance toTsTypeB :: ToTsType B where
  toTsType _ = TS.var $ TS.name "B"

instance toTsTypeC :: ToTsType C where
  toTsType _ = TS.var $ TS.name "C"

instance toTsTypeD :: ToTsType D where
  toTsType _ = TS.var $ TS.name "D"

instance toTsTypeE :: ToTsType E where
  toTsType _ = TS.var $ TS.name "E"

instance toTsTypeZ :: ToTsType Z where
  toTsType _ = TS.var $ TS.name "Z"

--------------------------------------------------------------------------------
-- class GenRecord
--------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> (Array (TS.Name /\ TS.Type))

instance genRecordNil :: GenRecord Nil where
  genRecord _ = []

instance genRecordCons :: (GenRecord rl, ToTsType t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ =
    genRecord (Proxy :: _ rl)
      # A.cons (TS.keyVal (reflectSymbol (Proxy :: _ s)) $ toTsType (undefined :: t))

--------------------------------------------------------------------------------
-- class GenVariant
--------------------------------------------------------------------------------

class GenVariant :: RowList Type -> Constraint
class GenVariant rl where
  genVariant :: Proxy rl -> TS.Type

instance genVariantNil :: (ToTsType t, IsSymbol s) => GenVariant (Cons s t Nil) where
  genVariant _ = TS.record'
    { tag: TS.tlString (reflectSymbol (Proxy :: _ s))
    , value: toTsType (Proxy :: _ t)
    }

else instance genVariantCons :: (GenVariant rl, ToTsType t, IsSymbol s) => GenVariant (Cons s t rl) where
  genVariant _ =
    genVariant (Proxy :: _ rl) |||
      TS.record'
        { tag: TS.tlString (reflectSymbol (Proxy :: _ s))
        , value: toTsType (Proxy :: _ t)
        }

--------------------------------------------------------------------------------

newtype Constructor a = Constructor a

instance toTsTypeConstructorFn :: ToTsType (Function a b) => ToTsType (Constructor (Function a b)) where
  toTsType (Constructor f) = TS.record' { create: toTsType f }

else instance toTsTypeConstructorVal :: ToTsType a => ToTsType (Constructor a) where
  toTsType (Constructor v) = TS.record' { value: toTsType v }


--------------------------------------------------------------------------------

newtype PredicateFn a = PredicateFn a 

class ToTsPredFn a where
  toTsPredFn :: TS.Type -> a -> TS.Type

instance toTsPredFnNil :: ToTsType a => ToTsPredFn (a -> Boolean) where
  toTsPredFn t _ = TS.function_
    [ TS.keyVal "obj" $ toTsType (undefined :: a) ]
    (TS.isPred (TS.name "obj") t)

else instance toTsPredicateFnRec :: (ToTsType a , ToTsPredFn b) => ToTsPredFn (a -> b) where
  toTsPredFn t _ = TS.function_
    [ TS.keyVal "_" $ toTsType (undefined :: a) ]
    (toTsPredFn t (undefined :: b))