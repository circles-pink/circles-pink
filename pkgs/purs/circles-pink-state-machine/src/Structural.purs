module Structural
  ( checkStructural
  , class Structural
  , class StructuralRL
  ) where

import Prelude

import Control.Promise (Promise)
import Data.BigInt (BigInt)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Variant (Variant)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.RowList (class RowToList, Cons, Nil)
import Type.Proxy (Proxy)
import Untagged.Union (OneOf)

--------------------------------------------------------------------------------
instance Structural Int

instance Structural String

instance Structural Char

instance Structural Boolean

instance Structural Number

instance Structural a => Structural (Array (a))

instance (RowToList r rl, StructuralRL rl) => Structural (Record r)

instance (Structural a, Structural b) => Structural (a -> b)

instance (Structural a1, Structural a2, Structural b) => Structural (Fn2 a1 a2 b)

instance (Structural a1, Structural b2, Structural a3, Structural b) => Structural (Fn3 a1 a2 a3 b)

instance (Structural a1, Structural a2, Structural a3, Structural a4, Structural b) => Structural (Fn4 a1 a2 a3 a4 b)

instance (RowToList r rl, StructuralRL rl) => Structural (Variant r)

instance (Structural a) => Structural (Promise a)

instance Structural Foreign

instance Structural a => Structural (Object a)

instance Structural BigInt

instance (Structural a, Structural b) => Structural (OneOf a b)

class Structural :: forall k. k -> Constraint
class Structural a

--------------------------------------------------------------------------------
instance StructuralRL Nil

instance (Structural t, StructuralRL rl) => StructuralRL (Cons s t rl)

class StructuralRL :: forall k. k -> Constraint
class StructuralRL rl

--------------------------------------------------------------------------------
-- | Static checking if a given type is structural
checkStructural :: forall a. Structural a => Proxy a -> Unit
checkStructural _ = unit
