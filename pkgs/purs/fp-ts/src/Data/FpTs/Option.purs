module Data.FpTs.Option
  ( None(..)
  , Option(..)
  , Some(..)
  , match
  , none
  , some
  ) where

import Prelude

import Data.FpTs.Union (type (-|-))
import Data.FpTs.Union as U
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import FpTs.Class (class FpTs)
import Type.Data.String (String', mkString')
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

newtype None = None { _tag :: String' "None" }

newtype Some a = Some { _tag :: String' "Some", value :: a }

newtype Option a = Option (None -|- Some a)

instance fpTsOption :: FpTs (Maybe a) (Option a) where
  toFpTs = M.maybe none some
  fromFpTs = match (const Nothing) Just

--------------------------------------------------------------------------------

none :: forall a. Option a
none = Option $ U.left $ None { _tag: mkString' (Proxy :: _ "None") }

some :: forall a. a -> Option a
some x = Option $ U.right $ Some { _tag: mkString' (Proxy :: _ "Some"), value: x }

match :: forall a b. (Unit -> b) -> (a -> b) -> Option a -> b
match onNone onSome (Option x) = case (unsafeCoerce x)._tag of
  "None" -> onNone unit
  "Some" -> onSome (unsafeCoerce x).value
  _ -> undefined