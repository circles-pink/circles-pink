module Simple.Data.Array where

import Prelude as P

map :: forall a b. (a -> b) -> Array a -> Array b
map = P.map