module StateMachine.Control where

import Prelude
import Data.Variant (case_, on)
import Stadium as S
import StateMachine.Protocol as P
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Env m
  = { apiCheckUserName :: String -> m Boolean
    , apiCheckEmail :: String -> m Boolean
    }

controller ::
  forall m.
  Monad m => Env m -> S.Control P.State P.Action m
controller env =
  S.mkController (Proxy :: _ P.Protocol) \setState msg -> do
    case_
      # on (Proxy :: _ "infoGeneral") (\_ -> pure unit)
      # on (Proxy :: _ "askUserName") (\{ username } -> pure unit)
      # on (Proxy :: _ "askEmail") (\{ email } -> pure unit)
      # undefined
