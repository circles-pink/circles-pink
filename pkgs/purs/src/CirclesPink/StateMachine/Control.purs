module CirclesPink.StateMachine.Control
  ( Env
  , circlesControl
  ) where

import Prelude
import CirclesPink.StateMachine (_circlesStateMachine)
import CirclesPink.StateMachine.Action (CirclesAction)
import CirclesPink.StateMachine.State (CirclesState)
import CirclesPink.StateMachine.State as S
import CirlesPink.StateMachine.Error (CirclesError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Type.Row (type (+))
import Undefined (undefined)

type Env m
  = { apiCheckUserName ::
        String ->
        ExceptV (CirclesError + ()) m { isValid :: Boolean }
    , apiCheckEmail :: String -> ExceptV (CirclesError + ()) m Boolean
    }

circlesControl :: forall m. MonadEffect m => Env m -> ((CirclesState -> CirclesState) -> m Unit) -> CirclesState -> CirclesAction -> m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { infoGeneral:
        { next: \set _ _ -> set $ \st -> S._askUsername st }
    , askUsername:
        { prev: \set _ _ -> set $ \st -> S._infoGeneral st
        , setUsername:
            \set _ x -> do
              log "hello1"
              set $ \st -> S._askUsername st { username = x }
              log "hello2"
              set
                $ \st ->
                    S._askUsername st { usernameApiResult = _loading :: RemoteData (Variant (CirclesError ())) { isValid :: Boolean } }
              log "hello3"
              result <- runExceptT $ env.apiCheckUserName x
              set
                $ \st ->
                    if x == st.username then case result of
                      Left e -> S._askUsername st { usernameApiResult = _failure e }
                      Right x -> S._askUsername st { usernameApiResult = _success x }
                    else
                      S._askUsername st
              pure unit
        }
    }
