module Simple.Data.Tuple where

import Data.Tuple (Tuple(..))

unTuple :: forall a b z. (a -> b -> z) -> Tuple a b -> z
unTuple onTuple (Tuple x y) = onTuple x y