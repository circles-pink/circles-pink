module Data.FpTs.Pair
  ( Pair
  ) where

import Data.Pair as P
import FpTs.Class (class FpTs, fromFpTs, toFpTs)

foreign import data Pair :: Type -> Type

foreign import mkPair :: forall a. a -> a -> Pair a

foreign import unMkPair :: forall a z. (a -> a -> z) -> Pair a -> z

instance fpTs :: (FpTs a a') => FpTs (P.Pair a) (Pair a') where
  toFpTs (P.Pair x y) = mkPair (toFpTs x) (toFpTs y)
  fromFpTs = unMkPair (\x y -> P.Pair (fromFpTs x) (fromFpTs y))