module CirlesPink.StateMachine.Error where

import Prelude
import Data.Variant (Variant)

type CirclesError'
  = ( errNetwork :: Unit
    , errService :: Unit
    )

type CirclesError
  = Variant CirclesError'
