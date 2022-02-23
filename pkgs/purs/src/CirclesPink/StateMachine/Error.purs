module CirlesPink.StateMachine.Error where

import Prelude
import Data.Variant (Variant)
import HTTP (NetworkError)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type CirclesError' r
  = ( errService :: Unit
    , errParse :: Unit
    | r
    )

type CirclesError r
  = NetworkError + CirclesError' + r
