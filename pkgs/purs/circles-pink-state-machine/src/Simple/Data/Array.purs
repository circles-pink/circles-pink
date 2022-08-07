module Simple.Data.Array where

import Prelude as P

mapArray :: forall a b. (a -> b) -> Array a -> Array b
mapArray = P.map