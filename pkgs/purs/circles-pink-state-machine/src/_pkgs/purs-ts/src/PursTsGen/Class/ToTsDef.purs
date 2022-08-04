module PursTsGen.Class.ToTsDef where

import PursTsGen.Prelude

import Data.Array (nub, (:))
import Data.Array.NonEmpty (NonEmptyArray, foldl1, toArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple, fst, snd)
import Data.Typelevel.Undefined (undefined)
import PursTsGen.Class.ToPursType (class ToPursType, toPursType)
import PursTsGen.Class.ToTsType (class ToTsType, toTsType)
import PursTsGen.Data.ABC (A(..), B)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL ((|||))
import PursTsGen.Lang.TypeScript.DSL as TS
import PursTsGen.Lang.TypeScript.Ops (resolveType)
import Type.Proxy (Proxy(..))

class ToTsDef :: forall k. k -> Constraint
class ToTsDef a where
  toTsDef :: Proxy a -> Array TS.Declaration

instance toTsDefMaybe :: ToTsDef (Maybe A) where
  toTsDef = genericToTsDef "Maybe"

instance toTsefEither :: ToTsDef (Either A B) where
  toTsDef = genericToTsDef "Either"

instance toTsDefTuple :: ToTsDef (Tuple A B) where
  toTsDef = genericToTsDef "Tuple"

instance toTsDefUnit :: ToTsDef Unit where
  toTsDef _ = pure $ TS.typeDef (TS.name "Unit") [] TS.undefined

instance toTsDefNullable :: ToTsDef (Nullable A) where
  toTsDef _ = pure $ TS.typeDef (TS.name "Nullable") [] (TS.null ||| toTsType A)

instance toTsTypeDefUserIdent :: ToTsDef Int where
  toTsDef _ = pure $ TS.typeDef (TS.name "Int") []
    $ TS.opaque (TS.qualName "Data_Int" "Int")
    $ TS.name
    <$> []

--------------------------------------------------------------------------------
-- class GenToTsDefSum
--------------------------------------------------------------------------------

class GenToTsDefSum :: forall k. k -> Constraint
class GenToTsDefSum rep where
  genToTsDefSum :: String -> Proxy rep -> NonEmptyArray (Array TS.Declaration /\ (PS.Type -> Array TS.Declaration) /\ TS.Type)

instance lastSum ::
  ( GenToTsDefProd t
  , IsSymbol s
  ) =>
  GenToTsDefSum (Constructor s t) where
  genToTsDefSum prefix _ = pure $ genToTsDefSum' prefix (Proxy :: _ s) (Proxy :: _ t)

else instance recSum ::
  ( IsSymbol s
  , GenToTsDefProd t
  , GenToTsDefSum rep
  ) =>
  GenToTsDefSum (Sum (Constructor s t) rep) where
  genToTsDefSum prefix _ = genToTsDefSum' prefix (Proxy :: _ s) (Proxy :: _ t) `NEA.cons` genToTsDefSum prefix (Proxy :: _ rep)

genToTsDefSum' :: forall s t. GenToTsDefProd t => IsSymbol s => String -> Proxy s -> Proxy t -> Array TS.Declaration /\ (PS.Type -> Array TS.Declaration) /\ TS.Type
genToTsDefSum' prefix _ _ = typeDef /\ ctor /\ type'
  where
  name = reflectSymbol (Proxy :: _ s)
  ctorType = TS.record [ TS.keyVal "name" $ TS.tlString name ]
  label = TS.keyVal "constructor" ctorType
  brand = TS.keyVal name TS.uniqueSymbol
  res = genToTsDefProd (undefined :: t) 0
  values = snd <$> res
  psTypes = fst <$> res
  types = snd <$> values
  typeDef =
    [ TS.emptyLine
    , TS.typeDef (TS.name (prefix <> "_" <> name)) [] $ TS.record (label : brand : values)
    ]
  { floating } = fold $ snd <<< resolveType <$> types
  type' = TS.mkType (TS.QualName Nothing (prefix <> "_" <> name)) $ TS.var <$> nub floating
  ctor ret =
    let
      pursType = PS.printType $ foldr PS.function ret psTypes
    in
      [ TS.emptyLine
      , TS.lineComment (name <> " :: " <> pursType)
      , TS.valueDef (TS.Name name) $ mkCtor type' $ types
      ]

mkCtor :: TS.Type -> Array TS.Type -> TS.Type
mkCtor t [] = TS.record' { value: t }
mkCtor t xs = TS.record' { create: foldr (\x y -> TS.function_ (pure (TS.name "_" /\ x)) y) t xs }

--------------------------------------------------------------------------------
-- class GenToTsDefProd
--------------------------------------------------------------------------------

class GenToTsDefProd rep where
  genToTsDefProd :: rep -> Int -> Array (PS.Type /\ (TS.Name /\ TS.Type))

instance nilProd :: GenToTsDefProd NoArguments where
  genToTsDefProd _ _ = []

instance nilOne :: (ToTsType t, ToPursType t) => GenToTsDefProd (Argument t) where
  genToTsDefProd _ i = [ toPursType (Proxy :: _ t) /\ (TS.keyVal ("value" <> show i) $ toTsType (undefined :: t)) ]

instance recProd :: (GenToTsDefProd a, GenToTsDefProd b) => GenToTsDefProd (Product a b) where
  genToTsDefProd _ i = genToTsDefProd (undefined :: a) i <> genToTsDefProd (undefined :: b) (i + 1)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

genericToTsDef' :: forall a rep. ToPursType a =>  GenToTsDefSum rep => String -> Proxy a -> Proxy rep -> Array TS.Declaration
genericToTsDef' name _ _ = 
  union : ctorTypes <> ((\f -> f $ toPursType (Proxy :: _ a)) =<< ctors)
  where
  xs = genToTsDefSum name (Proxy :: _ rep)
  ctorTypes = fst =<< toArray xs
  ctors = fst <<< snd <$> toArray xs
  union = TS.typeDef (TS.name name) [] $ foldl1 (|||) $ snd <<< snd <$> xs

genericToTsDef :: forall a rep. ToPursType a => Generic a rep => GenToTsDefSum rep => String -> Proxy a -> Array TS.Declaration
genericToTsDef name _ = genericToTsDef' name (Proxy :: _ a) (Proxy :: _ rep)

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

data MyThese a b = This a | That b | Both a b

derive instance genericMyThese :: Generic (MyThese a b) _

-- spec :: Spec Unit
-- spec = describe "CirclesPink.GenerateTSD.Generic" do
--   describe "genToTsDef" do
--     it "works for Tuples" do
--       genericToTsDef (Proxy :: _ (Tuple Boolean Number)) `shouldEqual`
--         ( TS.record'
--             { constructor: TS.record' { name: TS.tlString "Tuple" }
--             , value0: TS.boolean
--             , value1: TS.number
--             }
--         )
--     it "works for Maybe" do
--       genericToTsDef (Proxy :: _ (Maybe Number)) `shouldEqual`
--         ( TS.record'
--             { constructor: TS.record' { name: TS.tlString "Nothing" }
--             }
--             |||
--               TS.record'
--                 { constructor: TS.record' { name: TS.tlString "Just" }
--                 , value0: TS.number
--                 }

--         )
--     it "works for These" do
--       genericToTsDef (Proxy :: _ (MyThese Boolean Number)) `shouldEqual`
--         ( TS.record'
--             { constructor: TS.record' { name: TS.tlString "This" }
--             , value0: TS.boolean
--             }
--             ||| TS.record'
--               { constructor: TS.record' { name: TS.tlString "That" }
--               , value0: TS.number
--               }
--             |||
--               TS.record'
--                 { constructor: TS.record' { name: TS.tlString "Both" }
--                 , value0: TS.boolean
--                 , value1: TS.number
--                 }

--         )