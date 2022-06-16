module Data.Newtype.Extra
  ( (-|)
  , (|-)
  , applyAndUnwrap
  , unwrapAndApply
  )
  where

import Prelude
import Data.Newtype (class Newtype, unwrap)

--------------------------------------------------------------------------------
unwrapAndApply :: forall t a b. Newtype t a => t -> (a -> b) -> b
unwrapAndApply x f = f $ unwrap x


infixl 9 unwrapAndApply as -|

--------------------------------------------------------------------------------

applyAndUnwrap :: forall t a b. Newtype t a => (a -> b) -> t  -> b
applyAndUnwrap = flip unwrapAndApply 

infixr 9 applyAndUnwrap as |-
