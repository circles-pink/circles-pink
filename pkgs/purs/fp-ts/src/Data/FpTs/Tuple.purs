module Data.FpTs.Tuple
  ( Tuple
  , type (/\)
  )
  where

import Data.Tuple as P
import FpTs.Class (class FpTs)

foreign import data Tuple :: Type -> Type -> Type

infixr 6 type Tuple as /\

foreign import mkTuple :: forall a b. a -> b -> Tuple a b

foreign import unMkTuple :: forall a b z. (a -> b -> z) -> Tuple a b -> z

instance fpTs :: FpTs (P.Tuple a b) (Tuple a b) where
  toFpTs (P.Tuple x y) = mkTuple x y
  fromFpTs tpl = unMkTuple P.Tuple tpl