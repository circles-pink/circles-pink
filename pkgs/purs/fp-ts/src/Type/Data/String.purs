module Type.Data.String
  ( String'
  , mkString'
  , unString'
  )
  where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy)


newtype String' :: Symbol -> Type
newtype String' s = String' String

mkString' :: forall s. IsSymbol s => Proxy s -> String' s
mkString' s = String' $ reflectSymbol s

unString' :: forall s. String' s -> String
unString' (String' x) = x
