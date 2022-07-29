module Language.TypeScript.DTS.DSL
  ( (|||)
  , array
  , boolean
  , function
  , keyVal
  , mkType
  , null
  , number
  , opaque
  , record
  , record'
  , string
  , tlString
  , union
  , var
  )
  where

import Prelude
import Prim hiding (Type)

import Data.Bifunctor (lmap)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (fromHomogeneous)
import Foreign.Object as O
import Language.TypeScript.DTS (Name(..), QualName, Type(..))
import Type.Row.Homogeneous (class Homogeneous)

null :: Type
null = TypeNull

string :: Type
string = TypeString

number :: Type
number = TypeNumber

boolean :: Type
boolean = TypeBoolean

array :: Type -> Type
array = TypeArray

record :: Array (Name /\ Type) -> Type
record = TypeRecord

record' :: forall r. Homogeneous r Type => Record r -> Type
record' = TypeRecord <<< map (lmap Name) <<< O.toUnfoldable <<< fromHomogeneous

function :: Set Name -> Array (Name /\ Type) -> Type -> Type
function = TypeFunction

var :: Name -> Type
var = TypeVar

mkType :: QualName -> Array Type -> Type
mkType = TypeConstructor

opaque :: QualName -> Array Name -> Type
opaque = TypeOpaque

union :: Type -> Type -> Type
union = TypeUnion

infixr 6 union as |||

tlString :: String -> Type
tlString = TypeTLString


keyVal :: String -> Type -> Name /\ Type
keyVal n t = Name n /\ t