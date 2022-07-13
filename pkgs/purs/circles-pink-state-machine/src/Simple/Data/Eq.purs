module Simple.Data.Eq
  ( Eq
  )
  where

type Eq a = { eq :: a -> a -> Boolean }
