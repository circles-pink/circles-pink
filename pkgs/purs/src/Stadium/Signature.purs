module Stadium.Signature
  ( class Signature, showSignature
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Prim.Row (class Cons)

--------------------------------------------------------------------------------
class Signature a where
  showSignature :: Proxy a -> String

instance sigInt :: Signature Int where
  showSignature _ = "Int"
else instance sigUnit :: Signature Unit where
  showSignature _ = "Unit"
else instance sigString :: Signature String where
  showSignature _ = "String"
else instance sigOther :: Signature a where
  showSignature _ = "<signature not implemented yet>"
