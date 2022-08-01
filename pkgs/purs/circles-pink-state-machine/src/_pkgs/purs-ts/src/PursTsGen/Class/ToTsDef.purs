module PursTsGen.Class.ToTsDef where

import PursTsGen.Prelude

import Data.Array (intersperse, (:))
import Data.Array.NonEmpty (NonEmptyArray, foldl1, toArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple, fst, snd)
import Data.Typelevel.Undefined (undefined)
import PursTsGen.Class.ToTsType (class ToTsType, toTsType)
import PursTsGen.Data.ABC (A(..), B)
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

--------------------------------------------------------------------------------
-- class GenToTsDefSum
--------------------------------------------------------------------------------

class GenToTsDefSum :: forall k. k -> Constraint
class GenToTsDefSum rep where
  genToTsDefSum :: Proxy rep -> NonEmptyArray (TS.Declaration /\ TS.Declaration /\ TS.Type)

instance lastSum ::
  ( GenToTsDefProd t
  , IsSymbol s
  ) =>
  GenToTsDefSum (Constructor s t) where
  genToTsDefSum _ = pure $ genToTsDefSum' (Proxy :: _ s) (Proxy :: _ t)

else instance recSum ::
  ( IsSymbol s
  , GenToTsDefProd t
  , GenToTsDefSum rep
  ) =>
  GenToTsDefSum (Sum (Constructor s t) rep) where
  genToTsDefSum _ = genToTsDefSum' (Proxy :: _ s) (Proxy :: _ t) `NEA.cons` genToTsDefSum (Proxy :: _ rep)

genToTsDefSum' :: forall s t. GenToTsDefProd t => IsSymbol s => Proxy s -> Proxy t -> TS.Declaration /\ TS.Declaration /\ TS.Type
genToTsDefSum' _ _ = typeDef /\ ctor /\ type'
  where
  name = reflectSymbol (Proxy :: _ s)
  ctorType = TS.record [ TS.keyVal "name" $ TS.tlString name ]
  label = TS.keyVal "constructor" ctorType
  brand = TS.keyVal name TS.uniqueSymbol
  values = genToTsDefProd (undefined :: t) 0
  typeDef = TS.typeDef (TS.name name) [] $ TS.record (label : brand : values)
  { floating } = fold $ snd <<< resolveType <<< snd <$> values
  type' = TS.mkType (TS.QualName Nothing name) $ TS.var <$> floating
  ctor = TS.valueDef (TS.Name name) $ mkCtor type' $ snd <$> values

mkCtor :: TS.Type -> Array TS.Type -> TS.Type
mkCtor t [] = TS.record' { value: t }
mkCtor t xs = TS.record' { create: foldr (\x y -> TS.function_ (pure (TS.name "_" /\ x)) y) t xs }

--------------------------------------------------------------------------------
-- class GenToTsDefProd
--------------------------------------------------------------------------------

class GenToTsDefProd rep where
  genToTsDefProd :: rep -> Int -> Array (TS.Name /\ TS.Type)

instance nilProd :: GenToTsDefProd NoArguments where
  genToTsDefProd _ _ = []

instance nilOne :: ToTsType t => GenToTsDefProd (Argument t) where
  genToTsDefProd _ i = [ TS.keyVal ("value" <> show i) $ toTsType (undefined :: t) ]

instance recProd :: (GenToTsDefProd a, GenToTsDefProd b) => GenToTsDefProd (Product a b) where
  genToTsDefProd _ i = genToTsDefProd (undefined :: a) i <> genToTsDefProd (undefined :: b) (i + 1)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

genericToTsDef :: forall a rep. Generic a rep => GenToTsDefSum rep => String -> Proxy a -> Array TS.Declaration
genericToTsDef name _ =
  intersperse TS.emptyLine
    $ union : toArray ctors <> toArray ctorTypes
  where
  xs = genToTsDefSum $ (Proxy :: _ rep)
  ctorTypes = fst <$> xs
  ctors = fst <<< snd <$> xs
  union = TS.typeDef (TS.name name) [] $ foldl1 (|||) $ snd <<< snd <$> xs

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