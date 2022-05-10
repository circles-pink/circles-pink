module Data.Variant.Extra where

import Data.Variant (Variant)
import Unsafe.Coerce (unsafeCoerce)

getLabel :: forall r. Variant r -> String
getLabel v = (unsafeCoerce v).type
