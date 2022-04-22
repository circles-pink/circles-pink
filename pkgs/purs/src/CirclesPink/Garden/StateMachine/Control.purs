module CirclesPink.Garden.StateMachine.Control (circlesControl) where

import Prelude
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control.Env as Env
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
import CirclesPink.Garden.StateMachine.Control.States as States

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
    , submit:
        { prev: \set _ _ -> set \st -> S._magicWords st { direction = D._backwards }
        , submit: submitSubmit
        }
    , dashboard:
        { logout: \_ _ _ -> pure unit
        , getTrusts: dashboardGetTrusts
        }
    , login:
        { login: loginLogin
        , signUp: \set _ _ -> set \_ -> S.init
        , setMagicWords: \set _ words -> set \st -> S._login st { magicWords = words }
        }
    , trusts:
        { continue: \_ _ _ -> pure unit
        , getSafeStatus: trustsGetSafeStatus
        , finalizeRegisterUser: trustsFinalizeRegisterUser
        }
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
  -- InfoSecurity
  --------------------------------------------------------------------------------
  infoSecurityNext :: ActionHandler t m Unit S.UserData ( "magicWords" :: S.UserData )
  infoSecurityNext set _ _ = do
    result <- run $ env.generatePrivateKey
    case result of
      Right pk ->
        set \st ->
          if P.zeroKey == st.privateKey then
            S._magicWords st { privateKey = pk, direction = D._forwards }
          else
            S._magicWords st { direction = D._forwards }
      Left _ -> pure unit

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
  -- Login
  --------------------------------------------------------------------------------
  loginLogin :: ActionHandler t m Unit S.LoginState ( "login" :: S.LoginState, "trusts" :: S.TrustState, "dashboard" :: S.DashboardState )
  loginLogin set st _ = do
    let
      mnemonic = P.getMnemonicFromString st.magicWords

      privKey = P.mnemonicToKey mnemonic

      task :: ExceptV (Env.ErrUserResolve + Env.ErrGetSafeStatus + Env.ErrIsTrusted + Env.ErrTrustGetNetwork + Env.ErrIsTrusted + Env.ErrIsFunded + ()) _ _
      task = do
        user <- env.userResolve privKey
        safeStatus <- env.getSafeStatus privKey
        isTrusted <- env.isTrusted privKey <#> (\x -> x.isTrusted)
        trusts <-
          if isTrusted then
            pure []
          else
            env.trustGetNetwork privKey
        isReady' <- readyForDeployment env privKey
        pure { user, isTrusted, trusts, safeStatus, isReady: isReady' }
    results <- run' task
    case results of
      Left e -> set \st' -> S._login st' { error = pure e }
      Right { user, trusts, safeStatus }
        | safeStatus.isCreated && safeStatus.isDeployed ->
          set \_ ->
            S._dashboard
              { user
              , trusts
              , privKey
              , error: Nothing
              }
      Right { user, trusts, safeStatus, isReady } ->
        set \_ ->
          S._trusts
            { user
            , trusts
            , privKey
            , safeStatus
            , error: Nothing
            , isReady
            }

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

  --------------------------------------------------------------------------------
  -- Trusts
  --------------------------------------------------------------------------------
  trustsGetSafeStatus :: ActionHandler t m Unit S.TrustState ( "trusts" :: S.TrustState )
  trustsGetSafeStatus set st _ = do
    result <- run $ env.getSafeStatus st.privKey
    case result of
      Left e -> set \st' -> S._trusts st' { error = pure e }
      Right ss -> set \st' -> S._trusts st' { safeStatus = ss }

  trustsFinalizeRegisterUser :: ActionHandler t m Unit S.TrustState ( "trusts" :: S.TrustState, "dashboard" :: S.DashboardState )
  trustsFinalizeRegisterUser set st _ = do
    let
      task :: ExceptV (Env.ErrDeploySafe + Env.ErrDeployToken + ()) _ _
      task = do
        _ <- env.deploySafe st.privKey `catchError` \_ -> pure unit
        _ <- (env.deployToken st.privKey <#> const unit) `catchError` \_ -> pure unit
        _ <- env.deploySafe st.privKey
        _ <- (env.deployToken st.privKey <#> const unit)
        pure unit
    results <- run' task
    case results of
      Left e -> set \st' -> S._trusts st' { error = pure e }
      Right _ ->
        set \_ ->
          S._dashboard
            { user: st.user
            , trusts: st.trusts
            , privKey: st.privKey
            , error: Nothing
            }

  --------------------------------------------------------------------------------
  -- Submit
  --------------------------------------------------------------------------------
  submitSubmit :: ActionHandler t m Unit S.UserData ( "submit" :: S.UserData, "trusts" :: S.TrustState )
  submitSubmit set st _ = do
    let
      address = P.privKeyToAddress st.privateKey

      nonce = P.addressToNonce address
    result <-
      run do
        safeAddress <- env.getSafeAddress st.privateKey
        _ <- env.safePrepareDeploy st.privateKey
        env.userRegister
          st.privateKey
          { email: st.email
          , nonce
          , safeAddress
          , username: st.username
          }
        safeStatus <- env.getSafeStatus st.privateKey
        user <- env.userResolve st.privateKey
        trusts <- env.trustGetNetwork st.privateKey
        isReady' <- readyForDeployment env st.privateKey
        pure { safeStatus, user, trusts, isReady: isReady' }
    case result of
      Left e -> set \st' -> let _ = spy "e" e in S._submit st'
      Right { safeStatus, user, trusts, isReady } ->
        set \_ ->
          S._trusts
            { user
            , trusts
            , privKey: st.privateKey
            , safeStatus
            , error: Nothing
            , isReady
            }

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v
  = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit

--------------------------------------------------------------------------------
type ErrIsReady r
  = Env.ErrIsTrusted + Env.ErrIsFunded + r

readyForDeployment :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrIsReady r) m Boolean
readyForDeployment { isTrusted, isFunded } privKey = do
  isTrusted' <- isTrusted privKey <#> (\x -> x.isTrusted)
  isFunded' <- isFunded privKey
  pure (isTrusted' || isFunded')

--------------------------------------------------------------------------------
run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

run' :: forall t m e e' a. MonadTrans t => Nub e e' => Monad m => ExceptV e m a -> t m (Either (Variant e') a)
run' = lift <<< runExceptT'
  where
  runExceptT' :: ExceptV e m a -> m (Either (Variant e') a)
  runExceptT' = unsafeCoerce <<< runExceptT
