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

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Prim as P

--------------------------------------------------------------------------------

newtype Name = Name String

derive instance genericName :: Generic Name _
derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name

instance showName :: Show Name where
  show = genericShow

--------------------------------------------------------------------------------

data QualName = QualName (Maybe String) String

derive instance genericQualName :: Generic QualName _
derive instance eqQualName :: Eq QualName
derive instance ordQualName :: Ord QualName

instance showQualName :: Show QualName where
  show = genericShow

--------------------------------------------------------------------------------

newtype Path = Path String

--------------------------------------------------------------------------------

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
  | TypeUnion Type Type
  | TypeTLString String

derive instance genericType :: Generic Type _
derive instance eqType :: Eq Type
derive instance ordType :: Ord Type
instance showType :: Show Type where
  show x = genericShow x

--------------------------------------------------------------------------------

data Declaration
  = DeclTypeDef Name (Array Name) Type
  | DeclValueDef Name Type
  | DeclLineComment String
  | DeclEmptyLine

data Import = Import Name Path

data Module = Module ModuleHead ModuleBody

data ModuleHead = ModuleHead (Array String) (Array Import)

newtype ModuleBody = ModuleBody (Array Declaration)
