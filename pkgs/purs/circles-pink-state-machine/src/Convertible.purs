module Convertable where

class Convertible a b where
  convert :: a -> b

