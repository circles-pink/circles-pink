module CirclesPink.Garden.StateMachine.Control
  ( Env
  , RegisterError
  , circlesControl
  ) where

import Prelude
import CirclesPink.Garden.CirclesCore.Bindings (UserOptions)
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, ExceptT, lift, runExceptT)
import Data.Either (Either(..))
import Data.Variant (Variant, default, onMatch)
import Effect.Exception (Error)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Wallet.PrivateKey (PrivateKey)

type RegisterError
  = Variant ( errService :: Unit, errNative :: Error )

type Env m
  = { apiCheckUserName :: String -> ExceptT CirclesError m { isValid :: Boolean }
    , apiCheckEmail :: String -> ExceptT CirclesError m { isValid :: Boolean }
    , generatePrivateKey :: m PrivateKey
    , userRegister :: UserOptions -> ExceptT RegisterError m Unit
    }

circlesControl ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env m -> ((CirclesState -> CirclesState) -> t m Unit) -> CirclesState -> CirclesAction -> t m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { infoGeneral:
        { next:
            \set _ _ -> do
              set $ \st -> S._infoGeneral st { direction = D._forwards }
              set $ \st -> S._askUsername st
        }
    , askUsername:
        { prev:
            \set _ _ -> do
              set $ \st -> S._askUsername st { direction = D._backwards }
              set $ \st -> S._infoGeneral st
        , setUsername:
            \set _ username -> do
              set $ \st -> S._askUsername st { username = username }
              set
                $ \st ->
                    S._askUsername st { usernameApiResult = _loading :: RemoteData CirclesError { isValid :: Boolean } }
              result <- lift $ runExceptT $ env.apiCheckUserName username
              set
                $ \st ->
                    if username == st.username then case result of
                      Left e -> S._askUsername st { usernameApiResult = _failure e }
                      Right x -> S._askUsername st { usernameApiResult = _success x }
                    else
                      S._askUsername st
        , next:
            \set _ _ -> do
              set $ \st -> S._askUsername st { direction = D._forwards }
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
        { prev:
            \set _ _ -> do
              set $ \st -> S._askEmail st { direction = D._backwards }
              set $ \st -> S._askUsername st
        , setEmail:
            \set _ email -> do
              set $ \st -> S._askEmail st { email = email }
              set
                $ \st ->
                    S._askEmail st { emailApiResult = _loading :: RemoteData CirclesError { isValid :: Boolean } }
              result <- lift $ runExceptT $ env.apiCheckEmail email
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
            \set _ _ -> do
              set $ \st -> S._askEmail st { direction = D._forwards }
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
        { prev:
            \set _ _ -> set $ \st -> S._askEmail st { direction = D._backwards }
        , next:
            \set _ _ -> do
              set $ \st -> S._infoSecurity st { direction = D._forwards }
              pk <- lift $ env.generatePrivateKey
              set $ \st -> S._magicWords st { privateKey = pk }
        }
    , magicWords:
        { prev:
            \set _ _ -> do
              set $ \st -> S._magicWords st { direction = D._backwards }
              set $ \st -> S._infoSecurity st
        , newPrivKey:
            \set _ _ -> do
              pk <- lift $ env.generatePrivateKey
              set $ \st -> S._magicWords st { privateKey = pk }
        , next: \_ _ _ -> pure unit
        }
    , submit:
        { prev: \_ _ _ -> pure unit
        , submit: \_ _ _ -> pure unit
        }
    , dashboard:
        {}
    }
