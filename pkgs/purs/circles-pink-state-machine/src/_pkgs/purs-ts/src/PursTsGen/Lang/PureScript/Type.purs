module PursTsGen.Lang.PureScript.Type where

import Prelude
import Prim hiding (Type)

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as Set
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (fromHomogeneous)
import Foreign.Object as O
import Type.Row.Homogeneous (class Homogeneous)

data Type
  = TypeString
  | TypeNumber
  | TypeBoolean
  | TypeInt
  | TypeChar
  | TypeArray Type
  | TypeFunction Type Type
  | TypeRecord (Array (Name /\ Type))
  | TypeConstructor QualName (Array Type)
  | TypeVar Name
  | TypeQuantification (NonEmptySet Name) Type

newtype Name = Name String

data QualName = QualName (Maybe Name) Name

--------------------------------------------------------------------------------

string :: Type
string = TypeString

number :: Type
number = TypeNumber

boolean :: Type
boolean = TypeBoolean

int :: Type
int = TypeInt

char :: Type
char = TypeChar

array :: Type -> Type
array = TypeArray

record :: Array (Name /\ Type) -> Type
record = TypeRecord

function :: Type -> Type -> Type
function = TypeFunction

record' :: forall r. Homogeneous r Type => Record r -> Type
record' = TypeRecord <<< map (lmap Name) <<< O.toUnfoldable <<< fromHomogeneous

mkType :: QualName -> Array Type -> Type
mkType = TypeConstructor

mkType_ :: QualName -> Type
mkType_ qn = TypeConstructor qn []

var :: Name -> Type
var = TypeVar

forall' :: NonEmptySet Name -> Type -> Type
forall' = TypeQuantification

keyVal :: String -> Type -> Name /\ Type
keyVal n t = Name n /\ t

qualName :: String -> String -> QualName
qualName ns n = QualName (Just $ Name ns) (Name n)

qualName_ :: String -> QualName
qualName_ n = QualName Nothing (Name n)

name :: String -> Name
name n = Name n

--------------------------------------------------------------------------------
printType :: Type -> String
printType = case _ of
  TypeString -> "String"
  TypeNumber -> "Number"
  TypeBoolean -> "Boolean"
  TypeInt -> "Int"
  TypeChar -> "Char"
  TypeArray t -> "(Array " <> printType t <> ")"
  TypeFunction a b -> "(" <> printType a <> " -> " <> printType b <> ")"
  TypeRecord xs -> "{ " <> (joinWith "; " $ printRecordEntry <$> xs) <> " }"
  TypeConstructor qn ts -> "(" <> printQualName qn <> " " <> (joinWith " " $ printType <$> ts) <> ")"
  TypeVar n -> printName n
  TypeQuantification ns t -> "forall " <> (joinWith " " $ Set.toUnfoldable $ Set.map printName ns) <> ". " <> printType t
  where
  printRecordEntry (n /\ t) = printName n <> " :: " <> printType t

printName :: Name -> String
printName (Name s) = s

printQualName :: QualName -> String
printQualName (QualName (Just ns) n) = printName ns <> "." <> printName n
printQualName (QualName Nothing n) = printName n
