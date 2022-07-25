module Language.TypeScript.DTS.Print
  ( printModule
  ) where

import Prelude
import Prim hiding (Row, Type)

import Data.Array as A
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Language.TypeScript.DTS (Declaration(..), Import(..), Module(..), ModuleBody(..), ModuleHead(..), Name(..), Path(..), QualName(..), Type(..))

printType :: Type -> String
printType = case _ of
  TypeNull -> "null"
  TypeString -> "string"
  TypeNumber -> "number"
  TypeBoolean -> "boolean"
  TypeArray t -> "ReadonlyArray<" <> printType t <> ">"
  TypeRecord xs -> "Readonly<{ " <> printRecEntries xs <> " }>"
  TypeFunction targs args b -> printTargs targs <> printFnHead args <> printType b
  TypeVar n -> printName n
  TypeConstructor qn xs -> printQualName qn <> printTargs' xs
  TypeOpaque id targs -> "Readonly<{ readonly 'Opaque:" <> printQualName id <> "' : unique symbol; args : " <> printTargsArray targs <> "}>"
  TypeUnion xs -> joinWith " | " $ printType <$> xs
  TypeTLString s -> "\"" <> s <> "\""

  where
  printFnHead args = "(" <> printFnArgs args <> ") => "

  printFnArgs args = joinWith ", " $ printFnArg <$> args

  printFnArg (Name name /\ t) = name <> ": " <> printType t

  printRecEntries xs = joinWith "; " $ printRecEntry <$> xs

  printRecEntry (k /\ v) = printName k <> " : " <> printType v

  printTargsArray xs = "[" <> joinWith "," (printName <$> xs) <> "]"

printTargs :: Set Name -> String
printTargs xs | S.size xs == 0 = ""
printTargs xs = "<" <> joinWith "," (printName <$> S.toUnfoldable xs) <> ">"

printTargs' :: Array Type -> String
printTargs' xs | A.length xs == 0 = ""
printTargs' xs = "<" <> joinWith "," (printType <$> xs) <> ">"

printModule :: Module -> String
printModule (Module mh mb) = fold [ printModuleHead mh, printModuleBody mb ]

printModuleHead :: ModuleHead -> String
printModuleHead (ModuleHead im) | A.null im = ""
printModuleHead (ModuleHead im) = (joinWith "\n" $ printImport <$> im) <> "\n\n"

printModuleBody :: ModuleBody -> String
printModuleBody (ModuleBody xs) = joinWith "\n\n" $ printDeclaration <$> xs

printDeclaration :: Declaration -> String
printDeclaration = case _ of
  DeclTypeDef n targs t -> "export type " <> printName n <> " " <> printTargs targs <> " =  " <> printType t
  DeclValueDef n t -> "export const " <> printName n <> " : " <> printType t

printImport :: Import -> String
printImport (Import n p) = "import * as " <> printName n <> " from '" <> printPath p <> "'"

printName :: Name -> String
printName (Name s) = s

printQualName :: QualName -> String
printQualName (QualName (Just y) x) = y <> "." <> x
printQualName (QualName Nothing x) = x

printPath :: Path -> String
printPath (Path s) = s