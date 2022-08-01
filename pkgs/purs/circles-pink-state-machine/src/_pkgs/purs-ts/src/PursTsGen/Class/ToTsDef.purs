module PursTsGen.Class.ToTsDef where

import Prelude

import PursTsGen.Data.ABC (A(..), B)
import Data.Array ((:))
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import PursTsGen.Lang.TypeScript.DSL ((|||))
import PursTsGen.Lang.TypeScript.DSL as TS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import PursTsGen.Class.ToTsType (class ToTsType, toTsType)
import Type.Proxy (Proxy(..))

class ToTsDef :: forall k. k -> Constraint
class ToTsDef a where
  toTsDef :: Proxy a -> TS.Type

instance toTsTypeDefMaybe :: ToTsDef (Maybe A) where
  toTsDef = genericToTsDef

instance toTsTypeDefEither :: ToTsDef (Either A B) where
  toTsDef = genericToTsDef

instance toTsTypeDefTuple :: ToTsDef (Tuple A B) where
  toTsDef = genericToTsDef

instance toTsTypeDefUnit :: ToTsDef Unit where
  toTsDef _ = TS.undefined

instance toTsTypeDefNullable :: ToTsDef (Nullable A) where
  toTsDef _ = TS.null ||| toTsType A

--------------------------------------------------------------------------------
-- class GenToTsDefSum
--------------------------------------------------------------------------------

class GenToTsDefSum :: forall k. k -> Constraint
class GenToTsDefSum rep where
  genToTsDefSum :: Proxy rep -> TS.Type

instance lastSum ::
  ( GenToTsDefProd t
  , IsSymbol s
  ) =>
  GenToTsDefSum (Constructor s t) where
  genToTsDefSum _ = genToTsDefSum' (Proxy :: _ s) (Proxy :: _ t)

else instance recSum ::
  ( IsSymbol s
  , GenToTsDefProd t
  , GenToTsDefSum rep
  ) =>
  GenToTsDefSum (Sum (Constructor s t) rep) where
  genToTsDefSum _ = genToTsDefSum' (Proxy :: _ s) (Proxy :: _ t) ||| genToTsDefSum (Proxy :: _ rep)

genToTsDefSum' :: forall s t. GenToTsDefProd t => IsSymbol s => Proxy s -> Proxy t -> TS.Type
genToTsDefSum' _ _ = TS.record (label : brand : values)
  where
  name = reflectSymbol (Proxy :: _ s)
  label = TS.keyVal "constructor" ctor
  brand = TS.keyVal name TS.uniqueSymbol
  values = genToTsDefProd (undefined :: t) 0
  ctor = TS.record [ TS.keyVal "name" $ TS.tlString name ]

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

genericToTsDef :: forall a rep. Generic a rep => GenToTsDefSum rep => Proxy a -> TS.Type
genericToTsDef _ = genToTsDefSum $ (Proxy :: _ rep)

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

data MyThese a b = This a | That b | Both a b

derive instance genericMyThese :: Generic (MyThese a b) _

spec :: Spec Unit
spec = describe "CirclesPink.GenerateTSD.Generic" do
  describe "genToTsDef" do
    it "works for Tuples" do
      genericToTsDef (Proxy :: _ (Tuple Boolean Number)) `shouldEqual`
        ( TS.record'
            { constructor: TS.record' { name: TS.tlString "Tuple" }
            , value0: TS.boolean
            , value1: TS.number
            }
        )
    it "works for Maybe" do
      genericToTsDef (Proxy :: _ (Maybe Number)) `shouldEqual`
        ( TS.record'
            { constructor: TS.record' { name: TS.tlString "Nothing" }
            }
            |||
              TS.record'
                { constructor: TS.record' { name: TS.tlString "Just" }
                , value0: TS.number
                }

        )
    it "works for These" do
      genericToTsDef (Proxy :: _ (MyThese Boolean Number)) `shouldEqual`
        ( TS.record'
            { constructor: TS.record' { name: TS.tlString "This" }
            , value0: TS.boolean
            }
            ||| TS.record'
              { constructor: TS.record' { name: TS.tlString "That" }
              , value0: TS.number
              }
            |||
              TS.record'
                { constructor: TS.record' { name: TS.tlString "Both" }
                , value0: TS.boolean
                , value1: TS.number
                }

        )