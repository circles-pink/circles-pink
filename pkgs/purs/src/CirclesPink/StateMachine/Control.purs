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
import Data.Variant (Variant, default, onMatch)
import Effect.Class (class MonadEffect)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Type.Row (type (+))
import Wallet.PrivateKey (PrivateKey)

type Env m
  = { apiCheckUserName ::
        String ->
        ExceptV (CirclesError + ()) m { isValid :: Boolean }
    , apiCheckEmail ::
        String ->
        ExceptV (CirclesError + ()) m { isValid :: Boolean }
    -- , generatePrivateKey :: ExceptV () m PrivateKey
    -- , keyToMnemonic :: PrivateKey -> Array String
    -- , mnemonicToKey :: Array String -> PrivateKey
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
            \set _ username -> do
              set $ \st -> S._askUsername st { username = username }
              set
                $ \st ->
                    S._askUsername st { usernameApiResult = _loading :: RemoteData (Variant (CirclesError ())) { isValid :: Boolean } }
              result <- runExceptT $ env.apiCheckUserName username
              set
                $ \st ->
                    if username == st.username then case result of
                      Left e -> S._askUsername st { usernameApiResult = _failure e }
                      Right x -> S._askUsername st { usernameApiResult = _success x }
                    else
                      S._askUsername st
              pure unit
        , next:
            \set _ _ ->
              set
                $ \st ->
                    let
                      usernameValid =
                        default false
                          # onMatch
                              { success: (\r -> r.isValid) }
                    in
                      if usernameValid st.usernameApiResult then
                        S._askEmail st
                      else
                        S._askUsername st
        }
    , askEmail:
        { prev: \set _ _ -> set $ \st -> S._askUsername st
        , setEmail:
            \set _ email -> do
              set $ \st -> S._askEmail st { email = email }
              set
                $ \st ->
                    S._askEmail st { emailApiResult = _loading :: RemoteData (Variant (CirclesError ())) { isValid :: Boolean } }
              result <- runExceptT $ env.apiCheckEmail email
              set
                $ \st ->
                    if email == st.email then case result of
                      Left e -> S._askEmail st { emailApiResult = _failure e }
                      Right x -> S._askEmail st { emailApiResult = _success x }
                    else
                      S._askEmail st
              pure unit
        , setTerms: \set _ _ -> set $ \st -> S._askEmail st { terms = not st.terms }
        , setPrivacy: \set _ _ -> set $ \st -> S._askEmail st { privacy = not st.privacy }
        , next:
            \set _ _ ->
              set
                $ \st ->
                    let
                      emailValid =
                        default false
                          # onMatch
                              { success: (\r -> r.isValid) }
                    in
                      if (emailValid st.emailApiResult) && st.terms && st.privacy then
                        S._infoSecurity st
                      else
                        S._askEmail st
        }
    , infoSecurity:
        { prev: \set _ _ -> set $ \st -> S._askEmail st }
    }
