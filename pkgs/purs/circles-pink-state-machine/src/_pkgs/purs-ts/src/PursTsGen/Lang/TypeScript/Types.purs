module PursTsGen.Lang.TypeScript.Types where

import PursTsGen.Prelude
import Prim hiding (Row, Type)
import Prim as P

newtype Path = Path String

data Import = Import Name Path

data Module = Module ModuleHead ModuleBody

data ModuleHead = ModuleHead (Array String) (Array Import)

newtype ModuleBody = ModuleBody (Array Declaration)

data Token
  -- Keywords
  = TokConst
  | TokDefault
  | TokExport
  | TokDeclare
  | TokImport
  | TokVoid
  | TokReadonly
  | TokUnique
  | TokAs
  | TokFrom
  -- Puntuation
  | TokSemicolon
  | TokAsterisk
  | TokOpenParen
  | TokCloseParen
  | TokOpenBracket
  | TokCloseBracket
  | TokOpenBrace
  | TokCloseBrace
  | TokOpenAngle
  | TokCloseAngle
  | TokComma
  | TokEquals
  | TokColon
  --
  | TokWhitespace
  | TokNewline
  -- 
  | TokIdentifier String
  | TokStringLiteral String
  | TokNumberLiteral Number
  | TokSingleComment String
  | TokMultiComment String


--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------

newtype Name = Name String

derive instance genericName :: Generic Name _
derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name

instance showName :: Show Name where
  show = genericShow

--------------------------------------------------------------------------------
-- QualName
--------------------------------------------------------------------------------

data QualName = QualName (Maybe String) String

derive instance genericQualName :: Generic QualName _
derive instance eqQualName :: Eq QualName
derive instance ordQualName :: Ord QualName

instance showQualName :: Show QualName where
  show = genericShow

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data Type
  = TypeAny
  | TypeNull
  | TypeNever
  | TypeUndefined
  | TypeString
  | TypeNumber
  | TypeBoolean
  | TypeArray Type
  | TypeRecord (Array (Name /\ Type))
  | TypeFunction (Set Name) (Array (Name /\ Type)) Type
  | TypeVar Name
  | TypeConstructor QualName (Array Type)
  | TypeOpaque QualName (Array Name)
  | TypeUnion Type Type
  | TypeTLString String
  | TypeUniqueSymbol
  | TypeIsPred Name Type

derive instance genericType :: Generic Type _
derive instance eqType :: Eq Type
derive instance ordType :: Ord Type

instance showType :: Show Type where
  show x = genericShow x

--------------------------------------------------------------------------------
-- Declaration
--------------------------------------------------------------------------------

data Declaration
  = DeclTypeDef Name (Array Name) Type
  | DeclValueDef Name Type
  | DeclLineComment String
  | DeclEmptyLine
  | DeclUnsafeInline String
