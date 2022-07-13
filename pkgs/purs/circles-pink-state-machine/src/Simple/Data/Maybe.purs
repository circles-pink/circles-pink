module Simple.Data.Maybe
  ( bind
  , bindFlipped
  , eq
  , map
  , module Exp
  , pure
  ) where

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

