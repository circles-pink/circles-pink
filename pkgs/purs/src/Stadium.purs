module Stadium where

import Prelude
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Pair :: Type -> Symbol -> Type
type Pair a b
  = a /\ Proxy b

infixl 9 type Pair as ||
