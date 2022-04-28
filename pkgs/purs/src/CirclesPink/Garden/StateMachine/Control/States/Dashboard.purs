module CirclesPink.Garden.StateMachine.Control.States.Dashboard where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State (ErrDashboardStateGetTrustNetwork)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Either (Either(..))
import Data.Typelevel.Undefined (undefined)
import RemoteData (_failure, _success)

dashboard ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { logout :: ActionHandler t m Unit S.DashboardState ( "landing" :: S.LandingState )
  , getTrustNetwork :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  }
dashboard env =
  { logout: \_ _ _ -> pure unit
  , getTrustNetwork
  }
  where
  getTrustNetwork set st _ = do
    result <-
      (run' :: _ -> _ ErrDashboardStateGetTrustNetwork _ _)
        $ env.trustGetNetwork st.privKey
    case result of
      Left e -> set \st' -> S._dashboard st' { getTrustNetworkResult = _failure e }
      Right t -> set \st' -> S._dashboard st' { getTrustNetworkResult = _success t }
