module CirclesPink.Garden.StateMachine.Control.States.AskUsername where

import Prelude

import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Newtype.Extra ((-#))
import Data.Variant (default, onMatch)
import RemoteData (_failure, _loading, _success)

askUsername
  :: forall m
   . Monad m
  => Env.Env m
  -> CirclesConfig m
  -> { prev :: ActionHandler' m Unit S.UserData ("infoGeneral" :: S.UserData)
     , setUsername :: ActionHandler' m String S.UserData ("askUsername" :: S.UserData)
     , next :: ActionHandler' m Unit S.UserData ("askUsername" :: S.UserData, "askEmail" :: S.UserData, "infoSecurity" :: S.UserData)
     }
askUsername env cfg =
  { prev: \set _ _ -> set \st -> S._infoGeneral st { direction = D._backwards }
  , setUsername
  , next
  }
  where
  setUsername set _ username = do
    set \st -> S._askUsername st { username = username }
    set \st -> S._askUsername st { usernameApiResult = _loading unit }
    result <- runExceptT $ env.apiCheckUserName username
    set \st ->
      if username == st.username then case result of
        Left e -> S._askUsername st { usernameApiResult = _failure e }
        Right x -> S._askUsername st { usernameApiResult = _success x }
      else
        S._askUsername st

  next set st _ =
    let
      usernameValid =
        default false
          # onMatch
              { success: (\r -> r.isValid) }
    in
      if st.usernameApiResult -# usernameValid then
        case cfg -# _.extractEmail of
          Left e -> do
            set \st' -> S._askUsername st' { email = e }
            set \st' -> S._infoSecurity st' { direction = D._forwards }
          Right _ -> set \st' -> S._askEmail st' { direction = D._forwards }
      else
        set \st' -> S._askUsername st' { direction = D._forwards }
