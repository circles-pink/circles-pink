module Debug.Extra
  ( todo
  ) where

import Undefined (undefined)
import Prim.TypeError (class Warn, Text)

todo :: forall a. Warn (Text "Remove todo") => a
todo = undefined
