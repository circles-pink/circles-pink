module CirclesPink.GenerateTSD.Generic where

import CirclesPink.Prelude

import CirclesPink.GenerateTSD.Class (class ToTsType, toTsType)
import Data.Array (cons)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Language.TypeScript.DTS (Name, Type(..)) as DTS
import Language.TypeScript.DTS.DSL (boolean, keyVal, number, record', tlString, union) as DTS
import Language.TypeScript.DTS.DSL ((|||))

import Test.Spec (describe)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

genericToTsDef :: forall a rep. Generic a rep => GenToTsDefSum rep => Proxy a -> DTS.Type
genericToTsDef _ = genToTsDefSum $ (Proxy :: _ rep)

data MyThese a b = This a | That b | Both a b

derive instance genericMyThese :: Generic (MyThese a b) _

spec :: Spec Unit
spec = describe "CirclesPink.GenerateTSD.Generic" do
  describe "genToTsDef" do
    it "works for Tuples" do
      genericToTsDef (Proxy :: _ (Tuple Boolean Number)) `shouldEqual`
        ( DTS.record'
            { constructor: DTS.record' { name: DTS.tlString "Tuple" }
            , value0: DTS.boolean
            , value1: DTS.number
            }
        )
    it "works for Maybe" do
      genericToTsDef (Proxy :: _ (Maybe Number)) `shouldEqual`
        ( DTS.record'
            { constructor: DTS.record' { name: DTS.tlString "Nothing" }
            }
            |||
              DTS.record'
                { constructor: DTS.record' { name: DTS.tlString "Just" }
                , value0: DTS.number
                }

        )
    it "works for These" do
      genericToTsDef (Proxy :: _ (MyThese Boolean Number)) `shouldEqual`
        ( DTS.record'
            { constructor: DTS.record' { name: DTS.tlString "This" }
            , value0: DTS.boolean
            }
            ||| DTS.record'
              { constructor: DTS.record' { name: DTS.tlString "That" }
              , value0: DTS.number
              }
            |||
              DTS.record'
                { constructor: DTS.record' { name: DTS.tlString "Both" }
                , value0: DTS.boolean
                , value1: DTS.number
                }

        )

--------------------------------------------------------------------------------

class GenToTsDefSum :: forall k. k -> Constraint
class GenToTsDefSum rep where
  genToTsDefSum :: Proxy rep -> DTS.Type

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
  genToTsDefSum _ = genToTsDefSum' (Proxy :: _ s) (Proxy :: _ t) `DTS.union` genToTsDefSum (Proxy :: _ rep)

genToTsDefSum' :: forall s t. GenToTsDefProd t => IsSymbol s => Proxy s -> Proxy t -> DTS.Type
genToTsDefSum' _ _ = DTS.TypeRecord (label `cons` values)
  where
  label = DTS.keyVal "constructor" ctor
  values = genToTsDefProd (undefined :: t) 0
  ctor = DTS.TypeRecord [ DTS.keyVal "name" $ DTS.tlString (reflectSymbol (Proxy :: _ s)) ]

--------------------------------------------------------------------------------

class GenToTsDefProd rep where
  genToTsDefProd :: rep -> Int -> Array (DTS.Name /\ DTS.Type)

instance nilProd :: GenToTsDefProd NoArguments where
  genToTsDefProd _ _ = []

instance nilOne :: ToTsType t => GenToTsDefProd (Argument t) where
  genToTsDefProd _ i = [ DTS.keyVal ("value" <> show i) $ toTsType (undefined :: t) ]

instance recProd :: (GenToTsDefProd a, GenToTsDefProd b) => GenToTsDefProd (Product a b) where
  genToTsDefProd _ i = genToTsDefProd (undefined :: a) i <> genToTsDefProd (undefined :: b) (i + 1)

