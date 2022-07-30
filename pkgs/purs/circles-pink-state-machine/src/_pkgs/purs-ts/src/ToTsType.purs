module ToTsType where

import Prelude

import Data.ABC (A, B, C, D, E, Z)
import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Language.TypeScript.DTS.DSL ((|||))
import Language.TypeScript.DTS.DSL as DTS
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

class ToTsType a where
  toTsType :: a -> DTS.Type

instance toTsTypeNumber :: ToTsType Number where
  toTsType _ = DTS.number

instance toTsTypeString :: ToTsType String where
  toTsType _ = DTS.string

instance toTsTypeBoolean :: ToTsType Boolean where
  toTsType _ = DTS.boolean

instance toTsTypeArray :: ToTsType a => ToTsType (Array a) where
  toTsType _ = DTS.array $ toTsType (undefined :: a)

instance toTsTypeRecord :: (RowToList r rl, GenRecord rl) => ToTsType (Record r) where
  toTsType _ = DTS.record $ genRecord (Proxy :: _ rl)

instance toTsTypeFunction :: (ToTsType a, ToTsType b) => ToTsType (a -> b) where
  toTsType _ = DTS.function_
    [ DTS.keyVal "_" $ toTsType (undefined :: a) ]
    (toTsType (undefined :: b))

instance toTsTypeProxy :: ToTsType a => ToTsType (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

instance toTsTypeVariant :: (RowToList r rl, GenVariant rl) => ToTsType (Variant r) where
  toTsType _ = genVariant (Proxy :: _ rl)

instance toTsTypeMaybe :: ToTsType a => ToTsType (Maybe a) where
  toTsType _ = DTS.mkType (DTS.qualName "Data_Maybe" "Maybe")
    [ toTsType (Proxy :: _ a) ]

instance toTsTypeEither :: (ToTsType a, ToTsType b) => ToTsType (Either a b) where
  toTsType _ = DTS.mkType (DTS.qualName "Data_Either" "Either")
    [ toTsType (Proxy :: _ a), toTsType (Proxy :: _ b) ]

instance toTsTypeUnit :: ToTsType Unit where
  toTsType _ = DTS.mkType_ $ DTS.qualName "Data_Unit" "Unit"

instance toTsTypeNullable :: ToTsType a => ToTsType (Nullable a) where
  toTsType _ = DTS.null ||| toTsType (Proxy :: _ a)

instance toTsTypeA :: ToTsType A where
  toTsType _ = DTS.var $ DTS.name "A"

instance toTsTypeB :: ToTsType B where
  toTsType _ = DTS.var $ DTS.name "B"

instance toTsTypeC :: ToTsType C where
  toTsType _ = DTS.var $ DTS.name "C"

instance toTsTypeD :: ToTsType D where
  toTsType _ = DTS.var $ DTS.name "D"

instance toTsTypeE :: ToTsType E where
  toTsType _ = DTS.var $ DTS.name "E"

instance toTsTypeZ :: ToTsType Z where
  toTsType _ = DTS.var $ DTS.name "Z"

--------------------------------------------------------------------------------
-- class GenRecord
--------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> (Array (DTS.Name /\ DTS.Type))

instance genRecordNil :: GenRecord Nil where
  genRecord _ = []

instance genRecordCons :: (GenRecord rl, ToTsType t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ =
    genRecord (Proxy :: _ rl)
      # A.cons (DTS.keyVal (reflectSymbol (Proxy :: _ s)) $ toTsType (undefined :: t))

--------------------------------------------------------------------------------
-- class GenVariant
--------------------------------------------------------------------------------

class GenVariant :: RowList Type -> Constraint
class GenVariant rl where
  genVariant :: Proxy rl -> DTS.Type

instance genVariantNil :: (ToTsType t, IsSymbol s) => GenVariant (Cons s t Nil) where
  genVariant _ = DTS.record'
    { tag: DTS.tlString (reflectSymbol (Proxy :: _ s))
    , value: toTsType (Proxy :: _ t)
    }

else instance genVariantCons :: (GenVariant rl, ToTsType t, IsSymbol s) => GenVariant (Cons s t rl) where
  genVariant _ =
    genVariant (Proxy :: _ rl) |||
      DTS.record'
        { tag: DTS.tlString (reflectSymbol (Proxy :: _ s))
        , value: toTsType (Proxy :: _ t)
        }

