module Simple.Data.Either
  ( unEither
  ) where

import Data.Either (Either(..))

unEither :: forall a b z. { onLeft :: a -> z, onRight :: b -> z } -> Either a b -> z
unEither { onLeft, onRight } = case _ of
  Left x -> onLeft x
  Right x -> onRight x