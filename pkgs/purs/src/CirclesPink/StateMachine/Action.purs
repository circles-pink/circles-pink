module CirclesPink.StateMachine.Action
  ( CirclesAction
  ) where

import Prelude
import Data.Variant (Variant)

type CirclesAction
  = Variant
      ( state1 ::
          Variant
            ( action1 :: Int
            , action2 :: Boolean
            )
      , state2 :: Variant ()
      )
