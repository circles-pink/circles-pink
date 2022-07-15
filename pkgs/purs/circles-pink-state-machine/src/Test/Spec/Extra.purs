module Test.Spec.Extra
  ( describeFn
  ) where

import Prelude

import Test.Spec (Spec, describe)

foreign import unsafeFnName :: forall a b. (a -> b) -> String

describeFn :: forall a b c. (a -> b) -> Spec c -> Spec c
describeFn f = describe $ unsafeFnName f