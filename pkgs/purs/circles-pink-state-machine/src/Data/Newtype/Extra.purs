module Data.Newtype.Extra
  ( ($-)
  , (-#)
  , applyUnwrapped
  , applyUnwrappedFlipped
  ) where

import Prelude
import Data.Newtype (class Newtype, unwrap)

--------------------------------------------------------------------------------
applyUnwrappedFlipped :: forall t a b. Newtype t a => t -> (a -> b) -> b
applyUnwrappedFlipped x f = unwrap x # f

infixl 1 applyUnwrappedFlipped as -#

--------------------------------------------------------------------------------

applyUnwrapped :: forall t a b. Newtype t a => (a -> b) -> t -> b
applyUnwrapped f x = f $ unwrap x

infixr 0 applyUnwrapped as $-
