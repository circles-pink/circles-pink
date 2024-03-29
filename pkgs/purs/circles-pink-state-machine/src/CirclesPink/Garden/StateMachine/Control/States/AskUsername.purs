module CirclesPink.Garden.StateMachine.Control.States.AskUsername where

import Prelude

import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
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
  => EnvControl m
  -> CirclesConfig m
  -> { setUsername :: ActionHandler' m String S.UserData ("askUsername" :: S.UserData)
     , next :: ActionHandler' m Unit S.UserData ("askUsername" :: S.UserData, "askEmail" :: S.UserData, "infoSecurity" :: S.UserData)
     }
askUsername env cfg =
  { setUsername
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
            set \st' -> S._infoSecurity st' { direction = D.Forwards }
          Right _ -> set \st' -> S._askEmail st' { direction = D.Forwards }
      else
        set \st' -> S._askUsername st' { direction = D.Forwards }
