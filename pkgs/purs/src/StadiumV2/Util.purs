module StadiumV2.Util where

import Type.Data.List (Cons', List')

infixl 1 type Snoc' as <:

type Snoc' :: forall k. List' k -> k -> List' k
type Snoc' xs x
  = Cons' x xs

type App :: forall a b. (a -> b) -> a -> b
type App f a
  = f a

type AppFlipped :: forall a b. a -> (a -> b) -> b
type AppFlipped a f
  = f a

infixr 0 type App as $

infixl 1 type AppFlipped as #
