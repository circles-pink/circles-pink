module CirclesPink.Garden.StateMachine.Control.States.Trusts
  ( trusts
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', readyForDeployment, runExceptT')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (catchError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import RemoteData (_failure, _loading)

trusts
  :: forall m
   . Monad m
  => Env.Env m
  -> { getSafeStatus :: ActionHandler' m Unit S.TrustState ("trusts" :: S.TrustState)
     , finalizeRegisterUser :: ActionHandler' m Unit S.TrustState ("trusts" :: S.TrustState, "dashboard" :: S.DashboardState)
     }
trusts env =
  { getSafeStatus
  , finalizeRegisterUser
  }
  where
  getSafeStatus set st _ = do
    let
      task :: ExceptV S.ErrTrustState _ _
      task = do
        safeStatus <- env.getSafeStatus st.privKey
        isReady' <- readyForDeployment env st.privKey
        trusts' <- env.trustGetNetwork st.privKey
        pure { safeStatus, isReady: isReady', trusts: trusts' }
    results <- runExceptT' task
    case results of
      Left e -> set \st' -> S._trusts st' { trustsResult = _failure e }
      Right r -> set \st' -> S._trusts st' { safeStatus = r.safeStatus, isReady = r.isReady, trusts = r.trusts }

  finalizeRegisterUser set st _ = do
    set \st' -> S._trusts st' { trustsResult = _loading unit }
    let
      task :: ExceptV S.ErrTrustState _ _
      task = do
        -- First deploy always fails
        _ <- env.deploySafe st.privKey `catchError` \_ -> pure unit
        _ <- (env.deployToken st.privKey <#> const unit) `catchError` \_ -> pure unit
        -- Second deploy works
        _ <- env.deploySafe st.privKey
        _ <- (env.deployToken st.privKey <#> const unit)
        pure unit
    results <- runExceptT' task
    case results of
      Left e -> set \st' -> S._trusts st' { trustsResult = _failure e }
      Right _ ->
        set \_ ->
          S.initDashboard
            { user: st.user
            , privKey: st.privKey
            }
