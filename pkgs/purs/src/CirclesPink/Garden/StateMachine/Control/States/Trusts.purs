module CirclesPink.Garden.StateMachine.Control.States.Trusts where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, catchError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Type.Row (type (+))

trusts ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { getSafeStatus :: ActionHandler t m Unit S.TrustState ( "trusts" :: S.TrustState )
  , finalizeRegisterUser :: ActionHandler t m Unit S.TrustState ( "trusts" :: S.TrustState, "dashboard" :: S.DashboardState )
  }
trusts env =
  { getSafeStatus: getSafeStatus
  , finalizeRegisterUser: finalizeRegisterUser
  }
  where
  getSafeStatus set st _ = do
    result <- run $ env.getSafeStatus st.privKey
    case result of
      Left e -> set \st' -> S._trusts st' { error = pure e }
      Right ss -> set \st' -> S._trusts st' { safeStatus = ss }

  -- getSafeStatus set st _ = do
  --   let
  --     task :: ExceptV (Env.ErrGetSafeStatus + Env.ErrIsTrusted + Env.ErrIsFunded + ()) _ _
  --     task = do
  --       safeStatus <- env.getSafeStatus st.privKey
  --       isReady' <- readyForDeployment env st.privKey
  --       pure { safeStatus, isReady: isReady' }
  --   results <- run' task
  --   case results of
  --     Left e -> set \st' -> S._trusts st' { error = pure e }
  --     Right r -> set \st' -> S._trusts st' { safeStatus = r.safeStatus, isReady = r.isReady }
  --
  finalizeRegisterUser set st _ = do
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
