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
  forall ac m.
  Monad m => Env m -> S.Control P.State ac m
controller env =
  S.mkController (Proxy :: _ P.CirclesStateMachine) \setState msg -> do
    case_
      # on (Proxy :: _ "infoGeneral") (\_ -> pure unit)
      # on (Proxy :: _ "askUserName") (\{ username } -> pure unit)
      # on (Proxy :: _ "askEmail") (\{ email } -> pure unit)
