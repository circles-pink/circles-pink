module Language.TypeScript.DTS.Print
  ( printModule
  ) where

import Prelude
import Prim hiding (Row, Type)

import Data.Array as A
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Language.TypeScript.DTS (Declaration(..), Import(..), Module(..), ModuleBody(..), ModuleHead(..), Name(..), Path(..), QualName(..), Type(..))

printType :: Type -> String
printType = case _ of
  TypeNull -> "null"
  TypeUndefined -> "undefined"
  TypeString -> "string"
  TypeNumber -> "number"
  TypeBoolean -> "boolean"
  TypeArray t -> "ReadonlyArray<" <> printType t <> ">"
  TypeRecord xs -> "Readonly<{ " <> printRecEntries xs <> " }>"
  TypeFunction targs args b -> printTargs (S.toUnfoldable targs) <> printFnHead args <> printType b
  TypeVar n -> printName n
  TypeConstructor qn xs -> printQualName qn <> printTargs' xs
  TypeOpaque id targs -> "{ " <> printOpaque id <> printTargsValues targs <> " }"
  TypeUnion x y -> printType x <> " | " <> printType y
  TypeTLString s -> "\"" <> s <> "\""

  where
  printOpaque id = "readonly \"" <> "Opaque__" <> printQualName id <> "\": unique symbol;"

  printFnHead args = "(" <> printFnArgs args <> ") => "

  printFnArgs args = joinWith ", " $ printFnArg <$> args

  printFnArg (Name name /\ t) = name <> ": " <> printType t

  printRecEntries xs = joinWith "; " $ printRecEntry <$> xs

  printRecEntry (k /\ v) = printName k <> " : " <> printType v

  printTargsValues xs = xs # A.mapWithIndex (\i x -> Name ("_" <> show (i + 1)) /\ TypeVar x) # printRecEntries

printTargs :: Array Name -> String
printTargs xs | A.length xs == 0 = ""
printTargs xs = "<" <> joinWith "," (printName <$> xs) <> ">"

printTargs' :: Array Type -> String
printTargs' xs | A.length xs == 0 = ""
printTargs' xs = "<" <> joinWith "," (printType <$> xs) <> ">"

printModule :: Module -> String
printModule (Module mh mb) = fold [ printModuleHead mh, printModuleBody mb ]

printModuleHead :: ModuleHead -> String
printModuleHead (ModuleHead cm im) = printCmt <> printImports
  where
  printCmt | A.length cm == 0 = ""
  printCmt = (joinWith "\n" $ map (\c -> "// " <> c) cm) <> "\n\n"

  printImports | A.length im == 0 = ""
  printImports = (joinWith "\n" $ map printImport im) <> "\n"

printModuleBody :: ModuleBody -> String
printModuleBody (ModuleBody xs) = joinWith "\n" $ printDeclaration <$> xs

printDeclaration :: Declaration -> String
printDeclaration = case _ of
  DeclTypeDef n targs t -> "export type " <> printName n <> " " <> printTargs targs <> " =  " <> printType t
  DeclValueDef n t -> "export const " <> printName n <> " : " <> printType t
  DeclLineComment s -> "// " <> s
  DeclEmptyLine -> ""

printImport :: Import -> String
printImport (Import n p) = "import * as " <> printName n <> " from '" <> printPath p <> "'"

printName :: Name -> String
printName (Name s) = s

printQualName :: QualName -> String
printQualName (QualName (Just y) x) = y <> "." <> x
printQualName (QualName Nothing x) = x

printPath :: Path -> String
printPath (Path s) = s