module CirclesPink.GenerateTSD.Generic where

import Prelude

import CirclesPink.GenerateTSD.Class (class ToTsDef, toTsDef)
import Data.Array (cons)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic, Argument, Constructor(..), NoArguments(..), NoConstructors, Product, Sum, from)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Debug.Extra (todo)
import Language.TypeScript.DTS as DTS
import Type.Proxy (Proxy(..))

genericToTsDef :: forall a rep. Generic a rep => GenToTsDefSum rep => a -> DTS.Type
genericToTsDef = DTS.TypeUnion <<< genToTsDefSum <<< from

class GenToTsDefSum rep where
  genToTsDefSum :: rep -> NonEmptyArray DTS.Type

instance lastSum :: (IsSymbol s) => GenToTsDefSum (Sum (Constructor s t) NoConstructors) where
  genToTsDefSum = todo

else instance recSum :: (IsSymbol s, GenToTsDefSum rep) => GenToTsDefSum (Sum (Constructor s t) rep) where
  genToTsDefSum = todo

class GenToTsDefProd rep where
  genToTsDefProd :: rep -> Int -> Array (DTS.Name /\ DTS.Type)

instance nilProd :: GenToTsDefProd NoArguments where
  genToTsDefProd _ _ = []

instance recProd :: (ToTsDef t, GenToTsDefProd b) => GenToTsDefProd (Product (Argument t) b) where
  genToTsDefProd _ i =
    cons (DTS.Name ("value" <> show i) /\ toTsDef (undefined :: t))
      $ genToTsDefProd (undefined :: b) (i + 1)

