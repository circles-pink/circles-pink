module CirclesPink.Garden.StateMachine.Control.States.Login where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, TaskReturn, loginTask, run, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import RemoteData (RemoteData, _failure, _loading, _notAsked)
import Wallet.PrivateKey as P

login ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { login :: ActionHandler t m Unit S.LoginState ( "trusts" :: S.TrustState, "login" :: S.LoginState, "dashboard" :: S.DashboardState )
  , signUp :: ActionHandler t m Unit S.LoginState ( "infoGeneral" :: S.UserData )
  , setMagicWords :: ActionHandler t m String S.LoginState ( "login" :: S.LoginState )
  }
login env =
  { login: login'
  , signUp: \set _ _ -> set \_ -> S.init
  , setMagicWords: \set _ words -> set \st -> S._login st { magicWords = words }
  }
  where
  login' set st _ = do
    set \st' -> S._login st' { loginResult = _loading unit }
    let
      mnemonic = P.getMnemonicFromString st.magicWords

      privKey = P.mnemonicToKey mnemonic
    results <-
      (run' :: ExceptV (S.ErrLoginState) m TaskReturn -> _)
        $ do
            loginResult <- loginTask env privKey
            _ <- run $ env.saveSession privKey
            pure loginResult
    case results of
      Left e -> set \st' -> S._login st' { loginResult = _failure e }
      Right { user, safeStatus }
        | safeStatus.isCreated && safeStatus.isDeployed -> do
          set \_ ->
            S.initDashboard
              { user
              , privKey
              }
      Right { user, trusts, safeStatus, isReady } -> do
        set \_ ->
          S._trusts
            { user
            , trusts
            , privKey
            , safeStatus
            , trustsResult: _notAsked unit
            , isReady
            }
