module CirclesPink.Garden.StateMachine.Control.States.AskEmail where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Newtype.Extra ((-#), ($-))
import Data.Variant (default, onMatch)
import RemoteData (_failure, _loading, _success)

prev :: forall m. MonadCircles m => EnvControl m -> ActionHandler' m Unit S.UserData ("askUsername" :: S.UserData)
prev _ set _ _ = set \st -> S._askUsername st { direction = D._backwards }

setTerms :: forall m. MonadCircles m => EnvControl m -> ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData)
setTerms _ set _ _ = set \st -> S._askEmail st { terms = not st.terms }

setPrivacy :: forall m. MonadCircles m => EnvControl m -> ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData)
setPrivacy _ set _ _ = set \st -> S._askEmail st { privacy = not st.privacy }

next :: forall m. MonadCircles m => EnvControl m -> ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData, "infoSecurity" :: S.UserData)
next _ set st _ = do
  cfg <- ask
  let
    emailValid =
      default false
        # onMatch
            { success: (\r -> r.isValid) }

  if (emailValid $- st.emailApiResult) && st.terms && st.privacy then do
    case cfg -# _.extractEmail of
      Left _ -> pure unit
      Right cb -> cb st.email
    set \st' -> S._infoSecurity st' { direction = D._forwards }
  else
    set \st' -> S._askEmail st' { direction = D._forwards }

setEmail :: forall m. MonadCircles m => EnvControl m -> ActionHandler' m String S.UserData ("askEmail" :: S.UserData)
setEmail env set _ email = do
  set \st -> S._askEmail st { email = email }
  set \st -> S._askEmail st { emailApiResult = _loading unit }
  result <- runExceptT $ env.apiCheckEmail email
  set \st ->
    if email == st.email then case result of
      Left e -> S._askEmail st { emailApiResult = _failure e }
      Right x -> S._askEmail st { emailApiResult = _success x }
    else
      S._askEmail st
  pure unit
