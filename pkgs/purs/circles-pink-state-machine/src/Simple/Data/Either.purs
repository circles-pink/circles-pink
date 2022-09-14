module Simple.Data.Either
  ( mkEither
  , unEither
  )
  where

import Data.Either (Either(..))

unEither :: forall a b z. { onLeft :: a -> z, onRight :: b -> z } -> Either a b -> z
unEither { onLeft, onRight } = case _ of
  Left x -> onLeft x
  Right x -> onRight x

mkEither :: forall a b.
  { mkLeft :: a -> Either a b
  , mkRight :: b -> Either a b
  }
mkEither = {
  mkLeft: Left,
  mkRight: Right
}