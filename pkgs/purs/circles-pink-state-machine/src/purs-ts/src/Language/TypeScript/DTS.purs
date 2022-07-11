module Language.TypeScript.DTS
  ( Declaration(..)
  , Module(..)
  , Name(..)
  , Path(..)
  , QualName(..)
  , Type(..)
  , printModule
  ) where

import Prelude
import Prim hiding (Row, Type)

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))

newtype Name = Name String

data QualName = QualName (Maybe String) String

newtype Path = Path String

data Type
  = TypeString
  | TypeNumber
  | TypeBoolean
  | TypeArray Type
  | TypeRecord (Array (Name /\ Type))
  | TypeFunction (Array Name) (Array (Name /\ Type)) Type
  | TypeVar Name
  | TypeConstructor QualName (Array Type) 
  | TypeOpaque (Array Name)
  | TypeUnion (Array Type)
  | TypeTLString String

data Declaration
  = DeclTypeDef Name (Array Name) Type
  | DeclValueDef Name Type
  | DeclImport Name Path

newtype Module = Module (Array Declaration)

--------------------------------------------------------------------------------

printType :: Type -> String
printType = case _ of
  TypeString -> "string"
  TypeNumber -> "number"
  TypeBoolean -> "boolean"
  TypeArray t -> "ReadonlyArray<" <> printType t <> ">"
  TypeRecord xs -> "Readonly<{ " <> printRecEntries xs <> " }>"
  TypeFunction targs args b -> printTargs targs <> printFnHead args <> printType b
  TypeVar n -> printName n
  TypeConstructor qn xs -> printQualName qn <> printTargs' xs
  TypeOpaque targs -> "Readonly<{ readonly Opaque : unique symbol; args : " <> printTargsArray targs <> "}>"
  TypeUnion xs -> joinWith " | " $ printType <$> xs
  TypeTLString s -> "\"" <> s <> "\""

  where
  printFnHead args = "(" <> printFnArgs args <> ") => "

  printFnArgs args = joinWith ", " $ printFnArg <$> args

  printFnArg (Name name /\ t) = name <> ": " <> printType t

  printRecEntries xs = joinWith "; " $ printRecEntry <$> xs

  printRecEntry (k /\ v) = printName k <> " : " <> printType v

  printTargsArray xs = "[" <> joinWith "," (printName <$> xs) <> "]"

printTargs :: Array Name -> String
printTargs xs | A.length xs == 0 = ""
printTargs xs = "<" <> joinWith "," (printName <$> xs) <> ">"

printTargs' :: Array Type -> String
printTargs' xs | A.length xs == 0 = ""
printTargs' xs = "<" <> joinWith "," (printType <$> xs) <> ">"


printModule :: Module -> String
printModule (Module xs) = joinWith "\n\n" $ printDeclaration <$> xs

printDeclaration :: Declaration -> String
printDeclaration = case _ of
  DeclTypeDef n targs t -> "type " <> printName n <> " " <> printTargs targs <> " =  " <> printType t
  DeclValueDef n t -> "export const " <> printName n <> " : " <> printType t
  DeclImport n p -> "import * as " <> printName n <> " from " <> printPath p

printName :: Name -> String
printName (Name s) = s

printQualName :: QualName -> String
printQualName (QualName (Just y) x) = y <> "." <> x
printQualName (QualName Nothing x) = x

printPath :: Path -> String
printPath (Path s) = s