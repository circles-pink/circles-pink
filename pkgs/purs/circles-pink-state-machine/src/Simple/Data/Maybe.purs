module Simple.Data.Maybe
  ( bind
  , bindFlipped
  , eq
  , map
  , mkMaybe
  , module Exp
  , pure
  , unMaybe
  )
  where

import Prelude

import Control.Bind as B
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..), maybe, maybe') as Exp
import Prelude as P
import Simple.Data.Eq (Eq)

bind :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
bind = P.bind

bindFlipped :: forall a b. (a -> Maybe b) -> Maybe a -> Maybe b
bindFlipped = B.bindFlipped

map :: forall a b. (a -> b) -> Maybe a -> Maybe b
map = P.map

pure :: forall a. a -> Maybe a
pure = P.pure

eq :: forall a. Eq a -> Maybe a -> Maybe a -> Boolean
eq _EQ x y = case x, y of
  Just x', Just y' -> _EQ.eq x' y'
  _, _ -> false

unMaybe :: forall a z. { onJust :: a -> z, onNothing :: Unit -> z } -> Maybe a -> z
unMaybe { onJust, onNothing } = case _ of
  Just x -> onJust x
  Nothing -> onNothing unit

mkMaybe
  :: forall a
   . { mkJust :: a -> Maybe a
     , mkNothing :: Unit -> Maybe a
     }
mkMaybe =
  { mkNothing: \_ -> Nothing
  , mkJust: Just
  }