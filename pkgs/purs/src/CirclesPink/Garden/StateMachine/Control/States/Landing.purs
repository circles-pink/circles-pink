module CirclesPink.Garden.StateMachine.Control.States.Landing where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, loginTask, run, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Either (Either(..))
import Debug (spy)
import RemoteData (RemoteData, _failure, _loading, _success)
import Undefined (undefined)

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
      \set st ac -> do
        set \st' -> S._landing st' { checkSessionResult = _loading :: RemoteData S.ErrLandingStateResolved Unit }
        let
          task :: ExceptV S.ErrLandingState _ _
          task = do
            privKey <- env.restoreSession
            pure privKey
        result <- run' task
        case result of
          Left e -> set \st -> S._landing st { checkSessionResult = _failure e }
          Right pk -> undefined -- run $ loginTask env pk
  }
