module CirclesPink.GenerateTSD.Class where

import Prelude

import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.Data.UserIdent as CirclesPink.Data.UserIdent
import Data.ABC (A, B, C, D, E, Z)
import Data.Array as A
import Data.Either as Data.Either
import Data.Foldable (foldr)
import Data.IxGraph as Data.IxGraph
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant)
import Language.TypeScript.DTS (Declaration(..), Name(..), Type(..)) as DTS
import Language.TypeScript.DTS.DSL ((|||))
import Language.TypeScript.DTS.DSL (array, boolean, emptyLine, function_, keyVal, lineComment, mkType, mkType_, name, null, number, opaque, qualName, record, record', string, tlString, var) as DTS
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))

class ToTsType a where
  toTsType :: a -> DTS.Type

--toPursType :: a -> PT.Type

instance toTsTypeNumber :: ToTsType Number where
  toTsType _ = DTS.number

--toPursType _ = PT.number

instance toTsTypeString :: ToTsType String where
  toTsType _ = DTS.string

--toPursType _ = PT.string

instance toTsTypeBoolean :: ToTsType Boolean where
  toTsType _ = DTS.boolean

--toPursType _ = PT.boolean

instance toTsTypeArray :: ToTsType a => ToTsType (Array a) where
  toTsType _ = DTS.array $ toTsType (undefined :: a)

--toPursType _ = PT.array $ --toPursType (undefined :: a)

instance toTsTypeRecord :: (RowToList r rl, GenRecord rl) => ToTsType (Record r) where
  toTsType _ = DTS.record $ genRecord (Proxy :: _ rl)

--toPursType _ = PT.var $ PT.name "TODO"

instance toTsTypeFunction :: (ToTsType a, ToTsType b) => ToTsType (a -> b) where
  toTsType _ = DTS.function_
    [ DTS.keyVal "_" $ toTsType (undefined :: a) ]
    (toTsType (undefined :: b))

--toPursType _ = PT.function
-- (toPursType (undefined :: a))
-- (toPursType (undefined :: b))

instance toTsTypeProxy :: ToTsType a => ToTsType (Proxy a) where
  toTsType _ = toTsType (undefined :: a)

--toPursType _ = toPursType (undefined :: a)

instance toTsTypeVariant :: (RowToList r rl, GenVariant rl) => ToTsType (Variant r) where
  toTsType _ = genVariant (Proxy :: _ rl)

--toPursType _ = PT.var $ PT.name "TODO"

instance toTsTypeMaybe :: ToTsType a => ToTsType (Maybe a) where
  toTsType _ = DTS.mkType
    (DTS.qualName "Data_Maybe" "Maybe")
    [ toTsType (Proxy :: _ a) ]

--toPursType _ = PT.mkType
--(PT.qualName "Data_Maybe" "Maybe")
--[ toPursType (Proxy :: _ a) ]

instance toTsType_Data_Either_Either :: (ToTsType a, ToTsType b) => ToTsType (Data.Either.Either a b) where
  toTsType _ = DTS.mkType
    (DTS.qualName "Data_Either" "Either")
    [ toTsType (Proxy :: _ a), toTsType (Proxy :: _ b) ]

--toPursType _ = PT.mkType
--(PT.qualName "Data_Either" "Either")
--[ toPursType (Proxy :: _ a), toPursType (Proxy :: _ b) ]

instance toTsTypeUnit :: ToTsType Unit where
  toTsType _ = DTS.mkType_ $ DTS.qualName "Data_Unit" "Unit"

--toPursType _ = PT.mkType_ $ PT.qualName "Data_Unit" "Unit"

instance toTsType_Data_IxGraph_IxGraph :: (ToTsType id, ToTsType e, ToTsType n) => ToTsType (Data.IxGraph.IxGraph id e n) where
  toTsType _ = DTS.mkType (DTS.qualName "Data_IxGraph" "IxGraph")
    [ toTsType (Proxy :: _ id), toTsType (Proxy :: _ e), toTsType (Proxy :: _ n) ]

--toPursType _ = PT.mkType (PT.qualName "Data_IxGraph" "IxGraph")
--[ toPursType (Proxy :: _ id), toPursType (Proxy :: _ e), toPursType (Proxy :: _ n) ]

instance toTsType_CirclesPink_Data_Address_Address :: ToTsType (CirclesPink.Data.Address.Address) where
  toTsType _ = DTS.mkType_ $ DTS.qualName "CirclesPink_Data_Address" "Address"

--toPursType _ = PT.mkType_ $ PT.qualName "CirclesPink_Data_Address" "Address"

instance toTsType_CirclesPink_Data_Address_TrustNode :: ToTsType (CirclesPink.Data.TrustNode.TrustNode) where
  toTsType _ = DTS.mkType_ $ DTS.qualName "CirclesPink_Data_TrustNode" "TrustNode"

--toPursType _ = PT.mkType_ $ PT.qualName "CirclesPink_Data_TrustNode" "TrustNode"

instance toTsType_CirclesPink_Data_TrustConnection_TrustConnection :: ToTsType (CirclesPink.Data.TrustConnection.TrustConnection) where
  toTsType _ = DTS.mkType_ $ DTS.qualName "CirclesPink_Data_TrustConnection" "TrustConnection"

--toPursType _ = PT.mkType_ $ PT.qualName "CirclesPink_Data_TrustConnection" "TrustConnection"

instance toTsType_CirclesPink_Data_UserIdent_UserIdent :: ToTsType (CirclesPink.Data.UserIdent.UserIdent) where
  toTsType _ = DTS.mkType_ $ DTS.qualName "CirclesPink_Data_UserIdent" "UserIdent"

--toPursType _ = PT.mkType_ $ PT.qualName "CirclesPink_Data_UserIdent" "UserIdent"

instance toTsTypeNullable :: ToTsType a => ToTsType (Nullable a) where
  toTsType _ = DTS.null ||| toTsType (Proxy :: _ a)

--toPursType _ = PT.mkType (PT.qualName "Data_Nullable" "Nullable")
--[ toPursType (Proxy :: _ a) ]

instance toTsTypeA :: ToTsType A where
  toTsType _ = DTS.var $ DTS.name "A"

--toPursType _ = PT.var $ PT.name "a"

instance toTsTypeB :: ToTsType B where
  toTsType _ = DTS.var $ DTS.name "B"

--toPursType _ = PT.var $ PT.name "b"

instance toTsTypeC :: ToTsType C where
  toTsType _ = DTS.var $ DTS.name "C"

--toPursType _ = PT.var $ PT.name "c"

instance toTsTypeD :: ToTsType D where
  toTsType _ = DTS.var $ DTS.name "D"

--toPursType _ = PT.var $ PT.name "d"

instance toTsTypeE :: ToTsType E where
  toTsType _ = DTS.var $ DTS.name "E"

--toPursType _ = PT.var $ PT.name "e"

instance toTsTypeZ :: ToTsType Z where
  toTsType _ = DTS.var $ DTS.name "Z"

--toPursType _ = PT.var $ PT.name "z"

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

--------------------------------------------------------------------------------

-- class ToTsRef a where
--    toToTsRef :: a -> DTSTypeRef

class ToTsDef a where
  toTsDef :: a -> DTS.Type

instance toTsDefProxy :: ToTsDef a => ToTsDef (Proxy a) where
  toTsDef _ = toTsDef (undefined :: a)

instance toTsTypeDefMaybe :: ToTsDef (Maybe a) where
  toTsDef _ = DTS.opaque (DTS.qualName "Data_Maybe" "Maybe") $ DTS.name <$> [ "A" ]

-- genericToTsDef (Proxy :: _ (Maybe A))

instance toTsTypeDefEither :: ToTsDef (Data.Either.Either a b) where
  toTsDef _ = DTS.opaque (DTS.qualName "Data_Either" "Either") $ DTS.name <$> [ "A", "B" ]

instance toTsTypeDefUnit :: ToTsDef Unit where
  toTsDef _ = DTS.opaque (DTS.qualName "Data_Unit" "Unit") []

instance toTsTypeDef_Data_IxGraph_IxGraph :: ToTsDef (Data.IxGraph.IxGraph id e n) where
  toTsDef _ = DTS.opaque (DTS.qualName "Data_IxGraph" "IxGraph") $ DTS.Name <$> [ "Id", "E", "N" ]

instance toTsTypeDef_CirclesPink_Data_TrustNode :: ToTsDef (CirclesPink.Data.TrustNode.TrustNode) where
  toTsDef _ = DTS.opaque (DTS.qualName "CirclesPink_Data_TrustNode" "TrustNode") $ DTS.name <$> []

instance toTsTypeDef_CirclesPink_Data_Address :: ToTsDef (CirclesPink.Data.Address.Address) where
  toTsDef _ = DTS.opaque (DTS.qualName "CirclesPink_Data_Address" "Address") $ DTS.name <$> []

instance toTsTypeDef_CirclesPink_Data_TrustConnection :: ToTsDef (CirclesPink.Data.TrustConnection.TrustConnection) where
  toTsDef _ = DTS.opaque (DTS.qualName "CirclesPink_Data_TrustConnection" "TrustConnection") $ DTS.name <$> []

instance toTsTypeDef_CirclesPink_Data_UserIdent :: ToTsDef (CirclesPink.Data.UserIdent.UserIdent) where
  toTsDef _ = DTS.opaque (DTS.qualName "CirclesPink_Data_UserIdent" "UserIdent") $ DTS.name <$> []

-- class ToDTSTypeRef a where
--   toDTSTypeRef :: a -> DTSTypeRef

-- class ToDTSValueDef a where
--   toDTSValueDef :: a -> DTSValueDef

--------------------------------------------------------------------------------

newtype Ctor a = Ctor a

--------------------------------------------------------------------------------

val :: forall a. ToTsType a => a -> String -> Array DTS.Declaration
val x n =
  [ DTS.emptyLine
  , DTS.lineComment ("Value") -- <> (PT.printType $ toPursType x)
  , DTS.DeclValueDef (DTS.Name n) $ toTsType x
  ]

val' :: forall a. ToTsType a => Array DTS.Type -> a -> String -> Array DTS.Declaration
val' cs x n =
  [ DTS.emptyLine
  , DTS.DeclValueDef (DTS.Name n) $ foldr mkFn init cs
  ]
  where
  mkFn y f = DTS.TypeFunction S.empty [ (DTS.Name "_") /\ y ] f
  init = toTsType x

typ :: forall a. ToTsDef a => a -> String -> Array DTS.Declaration
typ x n =
  [ DTS.emptyLine
  , DTS.lineComment ("Type")
  , DTS.DeclTypeDef (DTS.Name n) mempty $ toTsDef x
  ]

cla :: forall dummy a. ToTsDef a => dummy -> a -> String -> Array DTS.Declaration
cla _ = typ

ins :: DTS.Type -> String -> Array DTS.Declaration
ins x n =
  [ DTS.emptyLine
  , DTS.lineComment "Instance"
  , DTS.DeclValueDef (DTS.Name n) $ x
  ]