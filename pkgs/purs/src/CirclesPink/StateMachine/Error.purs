module CirlesPink.StateMachine.Error where

import Prelude
import Data.Variant (Variant)

type CirclesError' r
  = ( errService :: Unit
    , errParse :: Unit
    | r
    )

type CirclesError
  = Variant
      ( errService :: Unit
      , errParse :: Unit
      , errNetwork :: Unit
      )
