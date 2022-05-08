module CirclesPink.Garden.StateMachine.Control.States.AskUsername where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Variant (default, onMatch)
import RemoteData (RemoteData, _failure, _loading, _success)

askUsername ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { prev :: ActionHandler t m Unit S.UserData ( "infoGeneral" :: S.UserData )
  , setUsername :: ActionHandler t m String S.UserData ( "askUsername" :: S.UserData )
  , next :: ActionHandler t m Unit S.UserData ( "askUsername" :: S.UserData, "askEmail" :: S.UserData )
  }
askUsername env =
  { prev: \set _ _ -> set \st -> S._infoGeneral st { direction = D._backwards }
  , setUsername
  , next
  }
  where
  setUsername set _ username = do
    set \st -> S._askUsername st { username = username }
    set \st -> S._askUsername st { usernameApiResult = _loading unit :: RemoteData Unit Unit CirclesError { isValid :: Boolean } }
    result <- run $ env.apiCheckUserName username
    set \st ->
      if username == st.username then case result of
        Left e -> S._askUsername st { usernameApiResult = _failure e }
        Right x -> S._askUsername st { usernameApiResult = _success x }
      else
        S._askUsername st

  next set _ _ =
    set \st ->
      let
        usernameValid =
          default false
            # onMatch
                { success: (\r -> r.isValid) }
      in
        if usernameValid $ unwrap st.usernameApiResult then
          S._askEmail st { direction = D._forwards }
        else
          S._askUsername st { direction = D._forwards }
