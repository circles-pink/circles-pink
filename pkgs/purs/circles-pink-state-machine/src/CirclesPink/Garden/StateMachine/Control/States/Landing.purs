module CirclesPink.Garden.StateMachine.Control.States.Landing where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', loginTask, runExceptT')
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import RemoteData (RemoteData, _failure, _loading, _notAsked)

landing
  :: forall m
   . Monad m
  => EnvControl m
  -> { signUp :: ActionHandler' m Unit S.LandingState ("infoGeneral" :: S.UserData)
     , signIn :: ActionHandler' m Unit S.LandingState ("login" :: S.LoginState)
     , checkForSession :: ActionHandler' m Unit S.LandingState ("landing" :: S.LandingState, "trusts" :: S.TrustState, "dashboard" :: S.DashboardState)
     }
landing env =
  { signUp: \set _ _ -> set \_ -> S.init
  , signIn: \set _ _ -> set \_ -> S.initLogin
  , checkForSession:
      \set _ _ -> do
        set \st' -> S._landing st' { checkSessionResult = _loading unit :: RemoteData Unit Unit S.ErrLandingStateResolved Unit }
        result <-
          (runExceptT' :: ExceptV S.ErrLandingState _ _ -> _) do
            privKey <- env.restoreSession
            loginResult <- loginTask env privKey
            pure { privKey, loginResult }
        case result of
          Left e -> set \st -> S._landing st { checkSessionResult = _failure e }
          Right { privKey, loginResult: { user, safeStatus } }
            | safeStatus.isCreated && safeStatus.isDeployed -> do
                set \_ ->
                  S.initDashboard
                    { user
                    , privKey
                    }
          Right { privKey, loginResult: { user, trusts, safeStatus, isReady } } -> do
            set \_ ->
              S._trusts
                { user
                , trusts
                , privKey
                , safeStatus
                , trustsResult: _notAsked unit
                , deploySafeResult: _notAsked unit
                , deployTokenResult: _notAsked unit
                , isReady
                }
  }
