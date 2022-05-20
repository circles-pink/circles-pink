module CirclesPink.Garden.StateMachine.Control.States.AskEmail where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Variant (default, onMatch)
import RemoteData (RemoteData, _failure, _loading, _success)

askEmail
  :: forall m
   . Monad m
  => Env.Env m
  -> { prev :: ActionHandler' m Unit S.UserData ("askUsername" :: S.UserData)
     , setEmail :: ActionHandler' m String S.UserData ("askEmail" :: S.UserData)
     , setTerms :: ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData)
     , setPrivacy :: ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData)
     , next :: ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData, "infoSecurity" :: S.UserData)
     }
askEmail env =
  { prev: \set _ _ -> set \st -> S._askUsername st { direction = D._backwards }
  , setEmail
  , setTerms: \set _ _ -> set \st -> S._askEmail st { terms = not st.terms }
  , setPrivacy: \set _ _ -> set \st -> S._askEmail st { privacy = not st.privacy }
  , next
  }
  where
  next set _ _ =
    set \st ->
      let
        emailValid =
          default false
            # onMatch
                { success: (\r -> r.isValid) }
      in
        if emailValid (unwrap st.emailApiResult) && st.terms && st.privacy then
          S._infoSecurity st { direction = D._forwards }
        else
          S._askEmail st { direction = D._forwards }

  setEmail set _ email = do
    set \st -> S._askEmail st { email = email }
    set \st -> S._askEmail st { emailApiResult = _loading unit :: RemoteData Unit Unit CirclesError { isValid :: Boolean } }
    result <- runExceptT $ env.apiCheckEmail email
    set \st ->
      if email == st.email then case result of
        Left e -> S._askEmail st { emailApiResult = _failure e }
        Right x -> S._askEmail st { emailApiResult = _success x }
      else
        S._askEmail st
    pure unit
