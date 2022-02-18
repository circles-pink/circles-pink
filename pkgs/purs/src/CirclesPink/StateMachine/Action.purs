module CirclesPink.StateMachine.Action
  ( CirclesAction
  ) where

import Prelude
import Data.Variant (Variant)

type CirclesAction
  = Variant
      ( infoGeneral ::
          Variant
            ( next :: Unit
            )
      , askUsername ::
          Variant
            ( prev :: Unit
            , setUsername :: String
            )
      )
