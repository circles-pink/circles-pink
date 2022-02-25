module CirlesPink.StateMachine.Error where

import Prelude

type CirclesError' r
  = ( errService :: Unit
    , errParse :: Unit
    | r
    )

type CirclesError
  = ( errService :: Unit
    , errParse :: Unit
    , errNetwork :: Unit
    )
