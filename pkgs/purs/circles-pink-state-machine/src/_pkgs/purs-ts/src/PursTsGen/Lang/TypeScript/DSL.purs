module PursTsGen.Lang.TypeScript.DSL
  ( (|||)
  , array
  , boolean
  , emptyLine
  , function
  , function_
  , keyVal
  , lineComment
  , mkType
  , mkType_
  , module Exp
  , name
  , never
  , null
  , number
  , opaque
  , qualName
  , qualName_
  , record
  , record'
  , string
  , tlString
  , typeDef
  , undefined
  , union
  , uniqueSymbol
  , valueDef
  , var
  ) where

import Prelude
import Prim hiding (Type)

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (fromHomogeneous)
import Foreign.Object as O
import PursTsGen.Lang.TypeScript (Declaration(..), Name(..), QualName(..), Type(..))
import PursTsGen.Lang.TypeScript.Types (Type(..), Name(..), QualName(..), Path(..), Declaration(..), Import(..), Module(..), ModuleHead(..), ModuleBody(..)) as Exp
import Type.Row.Homogeneous (class Homogeneous)

null :: Type
null = TypeNull

undefined :: Type
undefined = TypeUndefined

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

function_ :: Array (Name /\ Type) -> Type -> Type
function_ = TypeFunction S.empty

var :: Name -> Type
var = TypeVar

mkType :: QualName -> Array Type -> Type
mkType = TypeConstructor

mkType_ :: QualName -> Type
mkType_ qn = TypeConstructor qn []

opaque :: QualName -> Array Name -> Type
opaque = TypeOpaque

union :: Type -> Type -> Type
union = TypeUnion

infixr 6 union as |||

tlString :: String -> Type
tlString = TypeTLString

keyVal :: String -> Type -> Name /\ Type
keyVal n t = Name n /\ t

qualName :: String -> String -> QualName
qualName ns n = QualName (Just ns) n

qualName_ :: String -> QualName
qualName_ n = QualName Nothing n

uniqueSymbol :: Type
uniqueSymbol = TypeUniqueSymbol

name :: String -> Name
name n = Name n

lineComment :: String -> Declaration
lineComment = DeclLineComment

typeDef :: Name -> Array Name -> Type -> Declaration
typeDef = DeclTypeDef

valueDef :: Name -> Type -> Declaration
valueDef = DeclValueDef

never :: Type
never = TypeNever

emptyLine :: Declaration
emptyLine = DeclEmptyLine