module CirclesPink.GenerateTSD.Class where

import Prelude

import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.Data.UserIdent as CirclesPink.Data.UserIdent
import Data.ABC (A, B, C, D, E, Z)
import Data.Array as A
import Data.Either as Data.Either
import Data.IxGraph as Data.IxGraph
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Language.TypeScript.DTS as DTS
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

class ToTsType a where
  toTsType :: a -> DTS.Type

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
  toTsType _ = DTS.TypeFunction S.empty
    [ (DTS.Name "_") /\ toTsType (undefined :: a) ]
    (toTsType (undefined :: b))

instance toTsTypeProxy :: ToTsType a => ToTsType (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

instance toTsTypeVariant :: (RowToList r rl, GenVariant rl) => ToTsType (Variant r) where
  toTsType _ = DTS.TypeUnion $ genVariant (Proxy :: _ rl)

instance toTsTypeMaybe :: ToTsType a => ToTsType (Maybe a) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "Data_Maybe") "Maybe")
    [ toTsType (Proxy :: _ a) ]

instance toTsType_Data_Either_Either :: (ToTsType a, ToTsType b) => ToTsType (Data.Either.Either a b) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "Data_Either") "Either")
    [ toTsType (Proxy :: _ a), toTsType (Proxy :: _ b) ]

instance toTsTypeUnit :: ToTsType Unit where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "Data_Unit") "Unit")
    []

instance toTsType_Data_IxGraph_IxGraph :: (ToTsType id, ToTsType e, ToTsType n) => ToTsType (Data.IxGraph.IxGraph id e n) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "Data_IxGraph") "IxGraph")
    [ toTsType (Proxy :: _ id), toTsType (Proxy :: _ e), toTsType (Proxy :: _ n) ]

instance toTsType_CirclesPink_Data_Address_Address :: ToTsType (CirclesPink.Data.Address.Address) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "CirclesPink_Data_Address") "Address")
    []

instance toTsType_CirclesPink_Data_Address_TrustNode :: ToTsType (CirclesPink.Data.TrustNode.TrustNode) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "CirclesPink_Data_TrustNode") "TrustNode")
    []

instance toTsType_CirclesPink_Data_TrustConnection_TrustConnection :: ToTsType (CirclesPink.Data.TrustConnection.TrustConnection) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "CirclesPink_Data_TrustConnection") "TrustConnection")
    []

instance toTsType_CirclesPink_Data_UserIdent_UserIdent :: ToTsType (CirclesPink.Data.UserIdent.UserIdent) where
  toTsType _ = DTS.TypeConstructor
    (DTS.QualName (Just "CirclesPink_Data_UserIdent") "UserIdent")
    []

instance toTsTypeNullable :: ToTsType a => ToTsType (Nullable a) where
  toTsType _ = DTS.TypeUnion [ DTS.TypeNull, toTsType (Proxy :: _ a) ]

instance toTsTypeA :: ToTsType A where
  toTsType _ = DTS.TypeVar $ DTS.Name "A"

instance toTsTypeB :: ToTsType B where
  toTsType _ = DTS.TypeVar $ DTS.Name "B"

instance toTsTypeC :: ToTsType C where
  toTsType _ = DTS.TypeVar $ DTS.Name "C"

instance toTsTypeD :: ToTsType D where
  toTsType _ = DTS.TypeVar $ DTS.Name "D"

instance toTsTypeE :: ToTsType E where
  toTsType _ = DTS.TypeVar $ DTS.Name "E"

instance toTsTypeZ :: ToTsType Z where
  toTsType _ = DTS.TypeVar $ DTS.Name "Z"

--------------------------------------------------------------------------------

class GenRecord :: RowList Type -> Constraint
class GenRecord rl where
  genRecord :: Proxy rl -> (Array (DTS.Name /\ DTS.Type))

instance genRecordNil :: GenRecord Nil where
  genRecord _ = []

instance genRecordCons :: (GenRecord rl, ToTsType t, IsSymbol s) => GenRecord (Cons s t rl) where
  genRecord _ =
    genRecord (Proxy :: _ rl)
      # A.cons (DTS.Name (reflectSymbol (Proxy :: _ s)) /\ toTsType (undefined :: t))

--------------------------------------------------------------------------------

class GenVariant :: RowList Type -> Constraint
class GenVariant rl where
  genVariant :: Proxy rl -> (Array DTS.Type)

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
  toTsDef :: a -> DTS.Type

instance toTsDefProxy :: ToTsDef a => ToTsDef (Proxy a) where
  toTsDef _ = toTsDef (undefined :: a)

instance toTsTypeDefMaybe :: ToTsDef (Maybe a) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "Data_Maybe") "Maybe") $ DTS.Name <$> [ "A" ]

instance toTsTypeDefEither :: ToTsDef (Data.Either.Either a b) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "Data_Either") "Either") $ DTS.Name <$> [ "A", "B" ]

instance toTsTypeDefUnit :: ToTsDef Unit where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "Data_Unit") "Unit") []

instance toTsTypeDef_Data_IxGraph_IxGraph :: ToTsDef (Data.IxGraph.IxGraph id e n) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "Data_IxGraph") "IxGraph") $ DTS.Name <$> [ "Id", "E", "N" ]

instance toTsTypeDef_CirclesPink_Data_TrustNode :: ToTsDef (CirclesPink.Data.TrustNode.TrustNode) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "CirclesPink.Data.TrustNode") "TrustNode") $ DTS.Name <$> []

instance toTsTypeDef_CirclesPink_Data_Address :: ToTsDef (CirclesPink.Data.Address.Address) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "CirclesPink.Data.Address") "Address") $ DTS.Name <$> []

instance toTsTypeDef_CirclesPink_Data_TrustConnection :: ToTsDef (CirclesPink.Data.TrustConnection.TrustConnection) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "CirclesPink.Data.TrustConnection") "TrustConnection") $ DTS.Name <$> []

instance toTsTypeDef_CirclesPink_Data_UserIdent :: ToTsDef (CirclesPink.Data.UserIdent.UserIdent) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "CirclesPink.Data.UserIdent") "UserIdent") $ DTS.Name <$> []

-- class ToDTSTypeRef a where
--   toDTSTypeRef :: a -> DTSTypeRef

-- class ToDTSValueDef a where
--   toDTSValueDef :: a -> DTSValueDef

--------------------------------------------------------------------------------

val :: forall a. ToTsType a => a -> String -> DTS.Declaration
val x n = DTS.DeclValueDef (DTS.Name n) $ toTsType x

typ :: forall a. ToTsDef a => a -> String -> DTS.Declaration
typ x n = DTS.DeclTypeDef (DTS.Name n) mempty $ toTsDef x