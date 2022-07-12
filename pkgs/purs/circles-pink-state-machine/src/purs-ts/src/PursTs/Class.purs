module PursTs.Class where

import Prelude

import Data.Array as A
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Language.TypeScript.DTS as DTS
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

class ToTsType a where
  toTsType :: a -> DTS.Type Unit

instance toTsTypeNumber :: ToTsType Number where
  toTsType _ = DTS.TypeNumber

instance toTsTypeString :: ToTsType String where
  toTsType _ = DTS.TypeString

instance toTsTypeBoolean :: ToTsType Boolean where
  toTsType _ = DTS.TypeBoolean

instance toTsTypeArray :: ToTsType a => ToTsType (Array a) where
  toTsType _ = DTS.TypeArray $ toTsType (undefined :: a)

instance toTsTypeRecord :: (RowToList r rl, GenRecord rl) => ToTsType (Record r) where
  toTsType _ = DTS.TypeRecord $ genRecord (Proxy :: _ rl)

instance toTsTypeFunction :: (ToTsType a, ToTsType b) => ToTsType (a -> b) where
  toTsType _ = DTS.TypeFunction unit
    [ (DTS.Name "_") /\ toTsType (undefined :: a) ]
    (toTsType (undefined :: b))

instance toTsTypeProxy :: ToTsType a => ToTsType (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

instance toTsTypeVariant :: (RowToList r rl, GenVariant rl) => ToTsType (Variant r) where
  toTsType _ = DTS.TypeUnion $ genVariant (Proxy :: _ rl)

--------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> (Array (DTS.Name /\ DTS.Type Unit))

instance genRecordNil :: GenRecord Nil where
  genRecord _ = []

instance genRecordCons :: (GenRecord rl, ToTsType t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ =
    genRecord (Proxy :: _ rl)
      # A.cons (DTS.Name (reflectSymbol (Proxy :: _ s)) /\ toTsType (undefined :: t))

--------------------------------------------------------------------------------

class GenVariant :: RowList Type -> Constraint
class GenVariant rl where
  genVariant :: Proxy rl -> (Array (DTS.Type Unit))

instance genVariantNil :: GenVariant Nil where
  genVariant _ = []

instance genVariantCons :: (GenVariant rl, ToTsType t, IsSymbol s) => GenVariant (Cons s t rl) where
  genVariant _ =
    genVariant (Proxy :: _ rl)
      # A.cons
          ( DTS.TypeRecord
              [ DTS.Name "tag" /\ DTS.TypeTLString (reflectSymbol (Proxy :: _ s))
              , DTS.Name "value" /\ toTsType (Proxy :: _ t)
              ]
          )

--------------------------------------------------------------------------------

-- class ToTsRef a where
--    toToTsRef :: a -> DTSTypeRef

class ToTsDef a where
  toTsDef :: a -> DTS.Type Unit

instance toTsDefProxy :: ToTsDef a => ToTsDef (Proxy a) where
  toTsDef _ = toTsDef (undefined :: a)

-- class ToDTSTypeRef a where
--   toDTSTypeRef :: a -> DTSTypeRef

-- class ToDTSValueDef a where
--   toDTSValueDef :: a -> DTSValueDef
