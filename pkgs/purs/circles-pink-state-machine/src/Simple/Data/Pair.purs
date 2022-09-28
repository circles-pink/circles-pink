module Simple.Data.Pair where

import Data.Pair (Pair(..))

unPair :: forall a z. (a -> a -> z) -> Pair a -> z
unPair on (Pair x1 x2) = on x1 x2

mkPair :: forall a. a -> a -> Pair a
mkPair = Pair
