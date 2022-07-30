module ToTsDef where

import Prelude

import Data.ABC (A(..), B)
import Data.Array (cons, (:))
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product, Sum)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Undefined (undefined)
import Language.TypeScript.DTS.DSL ((|||))
import Language.TypeScript.DTS.DSL as DTS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import ToTsType (class ToTsType, toTsType)
import Type.Proxy (Proxy(..))

class ToTsDef :: forall k. k -> Constraint
class ToTsDef a where
  toTsDef :: Proxy a -> DTS.Type

instance toTsTypeDefMaybe :: ToTsDef (Maybe A) where
  toTsDef = genericToTsDef

instance toTsTypeDefEither :: ToTsDef (Either A B) where
  toTsDef = genericToTsDef

instance toTsTypeDefTuple :: ToTsDef (Tuple A B) where
  toTsDef = genericToTsDef

instance toTsTypeDefUnit :: ToTsDef Unit where
  toTsDef _ = DTS.undefined

instance toTsTypeDefNullable :: ToTsDef (Nullable A) where
  toTsDef _ = DTS.null ||| toTsType A

--------------------------------------------------------------------------------
-- class GenToTsDefSum
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
  genToTsDefSum _ = genToTsDefSum' (Proxy :: _ s) (Proxy :: _ t) ||| genToTsDefSum (Proxy :: _ rep)

genToTsDefSum' :: forall s t. GenToTsDefProd t => IsSymbol s => Proxy s -> Proxy t -> DTS.Type
genToTsDefSum' _ _ = DTS.record (label : values)
  where
  label = DTS.keyVal "constructor" ctor
  values = genToTsDefProd (undefined :: t) 0
  ctor = DTS.record [ DTS.keyVal "name" $ DTS.tlString (reflectSymbol (Proxy :: _ s)) ]

--------------------------------------------------------------------------------
-- class GenToTsDefProd
--------------------------------------------------------------------------------

class GenToTsDefProd rep where
  genToTsDefProd :: rep -> Int -> Array (DTS.Name /\ DTS.Type)

instance nilProd :: GenToTsDefProd NoArguments where
  genToTsDefProd _ _ = []

instance nilOne :: ToTsType t => GenToTsDefProd (Argument t) where
  genToTsDefProd _ i = [ DTS.keyVal ("value" <> show i) $ toTsType (undefined :: t) ]

instance recProd :: (GenToTsDefProd a, GenToTsDefProd b) => GenToTsDefProd (Product a b) where
  genToTsDefProd _ i = genToTsDefProd (undefined :: a) i <> genToTsDefProd (undefined :: b) (i + 1)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

genericToTsDef :: forall a rep. Generic a rep => GenToTsDefSum rep => Proxy a -> DTS.Type
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