module Language.TypeScript.DTS
  ( Declaration(..)
  , Import(..)
  , Module(..)
  , ModuleBody(..)
  , ModuleHead(..)
  , Name(..)
  , Path(..)
  , QualName(..)
  , Type(..)
  ) where

import Prelude
import Prim hiding (Row, Type)

import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\))
import Prim as P

newtype Name = Name String

derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name

data QualName = QualName (Maybe String) String

newtype Path = Path String

data Type
  = TypeNull
  | TypeString
  | TypeNumber
  | TypeBoolean
  | TypeArray Type
  | TypeRecord (Array (Name /\ Type))
  | TypeFunction (Set Name) (Array (Name /\ Type)) Type
  | TypeVar Name
  | TypeConstructor QualName (Array Type)
  | TypeOpaque QualName (Array Name)
  | TypeUnion (Array Type)
  | TypeTLString String

data Declaration
  = DeclTypeDef Name (Set Name) Type
  | DeclValueDef Name Type

data Import = Import Name Path

data Module = Module ModuleHead ModuleBody

newtype ModuleHead = ModuleHead (Array Import)

newtype ModuleBody = ModuleBody (Array Declaration)
