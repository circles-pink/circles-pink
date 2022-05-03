module CirclesPink.Garden.StateMachine.Control.States.Landing where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, loginTask, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import RemoteData (RemoteData, _failure, _loading, _notAsked)

landing ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { signUp :: ActionHandler t m Unit S.LandingState ( "infoGeneral" :: S.UserData )
  , signIn :: ActionHandler t m Unit S.LandingState ( "login" :: S.LoginState )
  , checkForSession :: ActionHandler t m Unit S.LandingState ( "landing" :: S.LandingState, "trusts" :: S.TrustState, "dashboard" :: S.DashboardState )
  }
landing env =
  { signUp: \set _ _ -> set \_ -> S.init
  , signIn: \set _ _ -> set \_ -> S.initLogin
  , checkForSession:
      \set _ _ -> do
        set \st' -> S._landing st' { checkSessionResult = _loading :: RemoteData S.ErrLandingStateResolved Unit }
        result <-
          (run' :: ExceptV S.ErrLandingState _ _ -> _) do
            privKey <- env.restoreSession
            loginResult <- loginTask env privKey
            pure { privKey, loginResult }
        case result of
          Left e -> set \st -> S._landing st { checkSessionResult = _failure e }
          Right { privKey, loginResult: { user, trusts, safeStatus } }
            | safeStatus.isCreated && safeStatus.isDeployed -> do
              set \_ ->
                S._dashboard
                  { user
                  , trusts
                  , privKey
                  , error: Nothing
                  , trustAddResult: _notAsked
                  , getBalanceResult: _notAsked
                  , checkUBIPayoutResult: _notAsked
                  , requestUBIPayoutResult: _notAsked
                  , getUsersResult: _notAsked
                  , transferResult: _notAsked
                  , userSearchResult: _notAsked
                  }
          Right { privKey, loginResult: { user, trusts, safeStatus, isReady } } -> do
            set \_ ->
              S._trusts
                { user
                , trusts
                , privKey
                , safeStatus
                , trustsResult: _notAsked
                , isReady
                }
  }
