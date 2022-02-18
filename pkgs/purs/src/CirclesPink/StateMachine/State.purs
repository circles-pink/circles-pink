module CirclesPink.StateMachine.State
  ( CirclesState
  ) where

import Prelude
import Data.Variant (Variant)

type CirclesState
  = Variant
      ( state1 :: Int
      , state2 :: String
      )
