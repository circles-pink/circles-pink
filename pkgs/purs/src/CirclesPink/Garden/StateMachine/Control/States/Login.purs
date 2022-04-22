module CirclesPink.Garden.StateMachine.Control.States.Login where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, readyForDeployment, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Type.Row (type (+))
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
  login' :: ActionHandler t m Unit S.LoginState ( "login" :: S.LoginState, "trusts" :: S.TrustState, "dashboard" :: S.DashboardState )
  login' set st _ = do
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
