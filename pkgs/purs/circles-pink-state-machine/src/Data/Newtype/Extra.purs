module Data.Newtype.Extra
  ( (-|)
  , unwrapAndGet
  ) where

import Prelude
import Data.Newtype (class Newtype, unwrap)

--------------------------------------------------------------------------------
unwrapAndGet :: forall t a b. Newtype t a => t -> (a -> b) -> b
unwrapAndGet x f = f $ unwrap x

infixl 9 unwrapAndGet as -|

--------------------------------------------------------------------------------
