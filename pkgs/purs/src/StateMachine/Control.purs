module StateMachine.Control where

import Prelude
import Data.Variant (case_, default, match, on, onMatch, inj)
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
  S.mkController (Proxy :: _ P.Protocol) \setState msg ->
    case_
      # onState (Proxy :: _ "infoGeneral") msg
          { "next": \_ -> setState $ inj (Proxy :: _ "askUserName") { username: "" }
          }
      # onState (Proxy :: _ "askUserName") msg
          { "prev": \_ -> pure unit
          , "next": \_ -> pure unit
          , "setUserName": \_ -> pure unit
          }

onState p msg x =
  on p
    ( \s ->
        msg
          # ( default (pure unit)
                # on p
                    ( default (pure unit)
                        # onMatch
                            x
                    )
            )
    )
