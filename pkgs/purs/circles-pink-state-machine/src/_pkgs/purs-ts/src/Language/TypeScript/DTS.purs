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
import Data.Tuple.Nested (type (/\))

newtype Name = Name String

derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name

data QualName = QualName (Maybe String) String

newtype Path = Path String

data Type a
  = TypeNull
  | TypeString
  | TypeNumber
  | TypeBoolean
  | TypeArray (Type a)
  | TypeRecord (Array (Name /\ Type a))
  | TypeFunction a (Array (Name /\ Type a)) (Type a)
  | TypeVar Name
  | TypeConstructor QualName (Array (Type a))
  | TypeOpaque QualName (Array Name)
  | TypeUnion (Array (Type a))
  | TypeTLString String

data Declaration a
  = DeclTypeDef Name a (Type a)
  | DeclValueDef Name (Type a)

data Import = Import Name Path

data Module a = Module ModuleHead (ModuleBody a)

newtype ModuleHead = ModuleHead (Array Import)

newtype ModuleBody a = ModuleBody (Array (Declaration a))
