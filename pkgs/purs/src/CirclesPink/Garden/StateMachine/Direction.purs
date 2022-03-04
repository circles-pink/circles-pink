module CirclesPink.Garden.StateMachine.Direction
  ( Direction
  , _backwards
  , _forwards
  ) where

import Prelude
import Data.Variant (Variant, inj)
import Type.Proxy (Proxy(..))

type Direction
  = Variant
      ( forwards :: Unit
      , backwards :: Unit
      )

_forwards ∷ Direction
_forwards = inj (Proxy :: _ "forwards") unit

_backwards ∷ Direction
_backwards = inj (Proxy :: _ "backwards") unit
