module Data.FpTs.Union
  ( Union
  , left
  , right
  , type (-|-)
  ) where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Union :: forall k. k -> k -> k

infixr 6 type Union as -|-

left :: forall a b. a -> Union a b
left = unsafeCoerce

right :: forall a b. b -> Union a b
right = unsafeCoerce
