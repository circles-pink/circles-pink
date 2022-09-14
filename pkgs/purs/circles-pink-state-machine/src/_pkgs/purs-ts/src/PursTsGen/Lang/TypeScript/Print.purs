module PursTsGen.Lang.TypeScript.Print
  ( printModule
  ) where

import Prim hiding (Row, Type)
import PursTsGen.Prelude

import Data.Array as A
import Data.Set as S
import Data.String (joinWith)
import PursTsGen.Lang.TypeScript.Types (Type(..), Declaration(..), Import(..), Module(..), ModuleBody(..), ModuleHead(..), Name(..), Path(..), QualName(..))

printType :: Type -> String
printType = case _ of
  TypeAny -> "any"
  TypeNull -> "null"
  TypeNever -> "never"
  TypeUndefined -> "undefined"
  TypeString -> "string"
  TypeNumber -> "number"
  TypeBoolean -> "boolean"
  TypeVar n -> printName n
  TypeArray t -> "ReadonlyArray<" <> printType t <> ">"
  TypeRecord xs -> "{ " <> printRecEntries xs <> " }"
  TypeFunction targs args b -> printTargs (S.toUnfoldable targs) <> printFnHead args <> printType b
  TypeConstructor qn xs -> printQualName qn <> printTargs' xs
  TypeOpaque id targs -> "{ " <> printOpaque id <> printTargsValues targs <> " }"
  TypeUnion x y -> printType x <> " | " <> printType y
  TypeTLString s -> "\"" <> s <> "\""
  TypeUniqueSymbol -> "unique symbol"
  TypeIsPred n t -> printName n <> " is " <> printType t

  where
  printOpaque id = "readonly \"" <> "Opaque__" <> printQualName id <> "\": unique symbol;"

  printFnHead args = "(" <> printFnArgs args <> ") => "

  printFnArgs args = joinWith ", " $ printFnArg <$> args

  printFnArg (Name name /\ t) = name <> ": " <> printType t

  printRecEntries xs = joinWith "; " $ printRecEntry <$> xs

  printRecEntry (k /\ v) = "readonly " <> printName k <> " : " <> printType v

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
  DeclUnsafeInline s -> s

printImport :: Import -> String
printImport (Import n p) = "import * as " <> printName n <> " from '" <> printPath p <> "'"

printName :: Name -> String
printName (Name s) = s

printQualName :: QualName -> String
printQualName (QualName (Just y) x) = y <> "." <> x
printQualName (QualName Nothing x) = x

printPath :: Path -> String
printPath (Path s) = s

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

-- spec :: Spec Unit
-- spec = do
--   describe "printName" do
--     it "prints a name" do
--       printName (Name "foo") `shouldEqual` "foo"

--   describe "printQualName" do
--     it "prints without namespace" do
--       QualName Nothing "foo" # printQualName # shouldEqual "foo"

--     it "prints with namespace" do
--       QualName (Just "namespace") "foo" # printQualName # shouldEqual "namespace.foo"

--   describe "printPath" do
--     it "prints the path" do
--       Path "foo" # printPath # shouldEqual "foo"

--   describe "printImport" do
--     it "" do
--       Import (Name "foo") (Path "bar") # printImport # shouldEqual "import * as foo from 'bar'"

--   describe "printType" do
--     describe "TypeNull" do
--       it "prints the null type" do
--         TypeNull # printType # shouldEqual "null"

--     describe "TypeUndefined" do
--       it "prints the undefined type" do
--         TypeUndefined # printType # shouldEqual "undefined"

--     describe "TypeString" do
--       it "prints the string type" do
--         TypeString # printType # shouldEqual "string"

--     describe "TypeNumber" do
--       it "prints the number type" do
--         TypeNumber # printType # shouldEqual "number"

--     describe "TypeBoolean" do
--       it "prints the boolean type" do
--         TypeBoolean # printType # shouldEqual "boolean"

--     describe "TypeVar" do
--       it "prints a type variable" do
--         TypeVar (Name "foo") # printType # shouldEqual "foo"

--     describe "TypeArray" do
--       it "prints an array type" do
--         TypeArray TypeNull # printType # shouldEqual "Array<null>"

--     describe "TypeRecord" do
--       it "prints an empty record type" do
--         TypeRecord [] # printType # shouldEqual "{}"

--       it "prints a record with fields of different types" do
--         TypeRecord
--           [ Name "foo" /\ TypeNull
--           , Name "bar" /\ TypeString
--           ]
--           # printType
--           # shouldEqual "{ foo : null; bar: string; }"

--     describe "TypeFunction" do
--       it "prints a type for a function without arguments" do
--         (printType $ TypeFunction S.empty [] TypeString) `shouldEqual` "() => string"

--       it "prints a type for a function with arguments of different types" do
--         TypeFunction S.empty
--           [ Name "foo" /\ TypeString
--           , Name "bar" /\ TypeNumber
--           ]
--           TypeString
--           # printType
--           # shouldEqual "(foo: string, bar: number) => string"

--       it "prints a type for a function with type arguments" do
--         TypeFunction (S.fromFoldable [ Name "foo", Name "bar" ]) [] TypeString
--           # printType
--           # shouldEqual "<foo, bar>() => string"

--     describe "TypeConstructor" do
--       it "prints a type constructor with arguments" do
--         TypeConstructor (QualName Nothing "foo") [ TypeString, TypeNumber ]
--           # printType
--           # shouldEqual "foo<string, number>"

--     describe "TypeOpaque" do
--       it "prints an opaque type without arguments" do
--         TypeOpaque (QualName Nothing "foo") []
--           # printType
--           # shouldEqual "{ readonly 'foo' : unique symbol; }"

--       it "prints an opaque type with arguments" do
--         TypeOpaque (QualName Nothing "foo") [ Name "foo", Name "bar" ]
--           # printType
--           # shouldEqual
--               "{ readonly 'Opaque__foo' : unique symbol; value0: foo, value1: bar; }"

--     describe "TypeUnion" do
--       it "prints an union type" do
--         TypeUnion TypeString TypeNumber # printType # shouldEqual "string | number"

--     describe "TypeTLString" do
--       it "prints a typelevel string" do
--         TypeTLString "foo" # printType # shouldEqual "'foo'"

--   describe "printModuleHead" do
--     it "..." do
--       ModuleHead [] [] # printModuleHead # shouldEqual ""

--     it "..." do
--       ModuleHead [ "foo", "bar" ] [ Import (Name "foo") (Path "bar") ]
--         # printModuleHead
--         # shouldEqual $ joinWith "\n" [ "// foo", "// bar", "import * as foo from 'bar'" ]

