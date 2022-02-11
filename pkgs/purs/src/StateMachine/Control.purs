module StateMachine.Control where

import Prelude
import Data.Variant (case_, default, inj, on, onMatch)
import Stadium as S
import StateMachine.Types as P
import Type.Proxy (Proxy(..))

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
          ( \_ ->
              { "next": \_ -> setState $ inj (Proxy :: _ "askUserName") { username: "" }
              }
          )
      # onState (Proxy :: _ "askUserName") msg
          ( \s' ->
              { "prev": \_ -> setState $ inj (Proxy :: _ "infoGeneral") unit
              , "next":
                  \_ -> do
                    isOk <- env.apiCheckUserName s'.username
                    if isOk then
                      setState
                        $ inj (Proxy :: _ "askEmail")
                            { email: ""
                            , privacy: false
                            , terms: false
                            , username: s'.username
                            }
                    else
                      pure unit
              , "setUserName": \n -> setState $ inj (Proxy :: _ "askUserName") { username: n }
              }
          )
      # onState (Proxy :: _ "askEmail") msg
          ( \s' ->
              { "prev": \_ -> setState $ inj (Proxy :: _ "askUserName") { username: s'.username }
              , "next":
                  \_ -> do
                    isOk <- env.apiCheckUserName s'.username
                    if isOk then
                      setState
                        $ inj (Proxy :: _ "infoSecurity")
                            { email: ""
                            , privacy: false
                            , terms: false
                            , username: s'.username
                            }
                    else
                      pure unit
              , "setEmail": \x -> setState $ inj (Proxy :: _ "askEmail") s' { email = x }
              , "setTerms": \x -> setState $ inj (Proxy :: _ "askEmail") s' { terms = x }
              , "setPrivacy": \x -> setState $ inj (Proxy :: _ "askEmail") s' { privacy = x }
              }
          )
      # onState (Proxy :: _ "infoSecurity") msg
          ( \s' ->
              { "prev": \_ -> setState $ inj (Proxy :: _ "askEmail") s'
              }
          )

onState p msg x =
  on p
    ( \s ->
        msg
          # ( default (pure unit)
                # on p
                    ( default (pure unit)
                        # onMatch
                            (x s)
                    )
            )
    )
