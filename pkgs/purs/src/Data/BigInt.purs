module Data.BigInt
  ( BigInt
  , fromString
  , toString
  ) where

foreign import data BigInt :: Type

foreign import fromString :: String -> BigInt

foreign import toString :: BigInt -> String
