module CirclesPink.Garden.StateMachine.Control.States.AskEmail where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Data.Either (Either(..))
import Data.Variant (default, onMatch)
import RemoteData (RemoteData, _failure, _loading, _success)

askEmail ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { prev :: ActionHandler t m Unit S.UserData ( "askUsername" :: S.UserData )
  , setEmail :: ActionHandler t m String S.UserData ( "askEmail" :: S.UserData )
  , setTerms :: ActionHandler t m Unit S.UserData ( "askEmail" :: S.UserData )
  , setPrivacy :: ActionHandler t m Unit S.UserData ( "askEmail" :: S.UserData )
  , next :: ActionHandler t m Unit S.UserData ( "askEmail" :: S.UserData, "infoSecurity" :: S.UserData )
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
        if emailValid st.emailApiResult && st.terms && st.privacy then
          S._infoSecurity st { direction = D._forwards }
        else
          S._askEmail st { direction = D._forwards }

  setEmail set _ email = do
    set \st -> S._askEmail st { email = email }
    set \st -> S._askEmail st { emailApiResult = _loading :: RemoteData CirclesError { isValid :: Boolean } }
    result <- run $ env.apiCheckEmail email
    set \st ->
      if email == st.email then case result of
        Left e -> S._askEmail st { emailApiResult = _failure e }
        Right x -> S._askEmail st { emailApiResult = _success x }
      else
        S._askEmail st
    pure unit
