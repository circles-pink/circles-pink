module CirclesPink.Garden.StateMachine.Control (circlesControl) where

import Prelude
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control.Common (readyForDeployment)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Control.States as States
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, catchError, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, default, onMatch)
import Debug (spy)
import Prim.Row (class Nub)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Wallet.PrivateKey (PrivateKey)
import Wallet.PrivateKey as P

circlesControl ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m -> ((S.CirclesState -> S.CirclesState) -> t m Unit) -> S.CirclesState -> CirclesAction -> t m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { landing: States.landing env
    , infoGeneral:
        { next: \set _ _ -> set \st -> S._askUsername st { direction = D._forwards }
        }
    , askUsername:
        { prev: \set _ _ -> set \st -> S._infoGeneral st { direction = D._backwards }
        , setUsername: askUsernameSetUsername
        , next: askUsernameNext
        }
    , askEmail:
        { prev: \set _ _ -> set \st -> S._askUsername st { direction = D._backwards }
        , setEmail: askEmailSetEmail
        , setTerms: \set _ _ -> set \st -> S._askEmail st { terms = not st.terms }
        , setPrivacy: \set _ _ -> set \st -> S._askEmail st { privacy = not st.privacy }
        , next: askEmailNext
        }
    , infoSecurity: States.infoSecurity env
    , magicWords:
        { prev: \set _ _ -> set \st -> S._infoSecurity st { direction = D._backwards }
        , newPrivKey: magicWordsNewPrivateKey
        , next: \set _ _ -> set \st -> S._submit st { direction = D._forwards }
        }
    , submit: States.submit env
    , dashboard:
        { logout: \_ _ _ -> pure unit
        , getTrusts: dashboardGetTrusts
        }
    , login: States.login env
    , trusts: States.trusts env
    , debug:
        { coreToWindow: debugCoreToWindow
        }
    }
  where
  --------------------------------------------------------------------------------
  -- AskUsername
  --------------------------------------------------------------------------------
  askUsernameSetUsername :: ActionHandler t m String S.UserData ( "askUsername" :: S.UserData )
  askUsernameSetUsername set _ username = do
    set \st -> S._askUsername st { username = username }
    set \st -> S._askUsername st { usernameApiResult = _loading :: RemoteData CirclesError { isValid :: Boolean } }
    result <- run $ env.apiCheckUserName username
    set \st ->
      if username == st.username then case result of
        Left e -> S._askUsername st { usernameApiResult = _failure e }
        Right x -> S._askUsername st { usernameApiResult = _success x }
      else
        S._askUsername st

  askUsernameNext :: ActionHandler t m Unit S.UserData ( "askUsername" :: S.UserData, "askEmail" :: S.UserData )
  askUsernameNext set _ _ =
    set \st ->
      let
        usernameValid =
          default false
            # onMatch
                { success: (\r -> r.isValid) }
      in
        if usernameValid st.usernameApiResult then
          S._askEmail st { direction = D._forwards }
        else
          S._askUsername st { direction = D._forwards }

  --------------------------------------------------------------------------------
  -- AskEmail
  --------------------------------------------------------------------------------
  askEmailNext :: ActionHandler t m Unit S.UserData ( "askEmail" :: S.UserData, "infoSecurity" :: S.UserData )
  askEmailNext set _ _ =
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

  askEmailSetEmail :: ActionHandler t m String S.UserData ( "askEmail" :: S.UserData )
  askEmailSetEmail set _ email = do
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

  --------------------------------------------------------------------------------
  -- MagicWords
  --------------------------------------------------------------------------------
  magicWordsNewPrivateKey :: ActionHandler t m Unit S.UserData ( "magicWords" :: S.UserData )
  magicWordsNewPrivateKey set _ _ = do
    result <- run $ env.generatePrivateKey
    case result of
      Right pk -> set \st -> S._magicWords st { privateKey = pk }
      Left _ -> pure unit

  --------------------------------------------------------------------------------
  -- Debug
  --------------------------------------------------------------------------------
  debugCoreToWindow :: ActionHandler t m Unit S.DebugState ( "debug" :: S.DebugState )
  debugCoreToWindow _ st _ = do
    let
      mnemonic = P.getMnemonicFromString st.magicWords
    let
      privKey = P.mnemonicToKey mnemonic
    _ <- run $ env.coreToWindow privKey
    pure unit

  --------------------------------------------------------------------------------
  -- Dashbaord
  --------------------------------------------------------------------------------
  dashboardGetTrusts :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  dashboardGetTrusts set st _ = do
    result <- run $ env.trustGetNetwork st.privKey
    case result of
      Left e -> set \st' -> S._dashboard st' { error = pure e }
      Right t -> set \st' -> S._dashboard st' { trusts = t }

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v
  = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit

--------------------------------------------------------------------------------
run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

run' :: forall t m e e' a. MonadTrans t => Nub e e' => Monad m => ExceptV e m a -> t m (Either (Variant e') a)
run' = lift <<< runExceptT'
  where
  runExceptT' :: ExceptV e m a -> m (Either (Variant e') a)
  runExceptT' = unsafeCoerce <<< runExceptT
