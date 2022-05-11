module CirclesPink.Garden.StateMachine.Error where

import Prelude
import Data.Variant (Variant)
import HTTP (Req)

type CirclesError' r
  = ( errService :: Unit
    , errParse :: Unit
    | r
    )

type CirclesError
  = Variant
      ( errService :: Unit
      , errParse :: Unit
      , errNetwork :: Req
      , errParseJson :: Unit
      )
