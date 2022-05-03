module CirclesPink.Garden.StateMachine.Control.States.Trusts
  ( trusts
  ) where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, readyForDeployment, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, catchError)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import RemoteData (RemoteData, _failure, _loading, _notAsked)

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
        pure { safeStatus, isReady: isReady' }
    results <- run' task
    case results of
      Left e -> set \st' -> S._trusts st' { trustsResult = _failure e }
      Right r -> set \st' -> S._trusts st' { safeStatus = r.safeStatus, isReady = r.isReady }

  finalizeRegisterUser set st _ = do
    set \st' -> S._trusts st' { trustsResult = _loading :: RemoteData _ _ }
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
    results <- run' task
    case results of
      Left e -> set \st' -> S._trusts st' { trustsResult = _failure e }
      Right _ ->
        set \_ ->
          S._dashboard
            { user: st.user
            , trusts: st.trusts
            , privKey: st.privKey
            , error: Nothing
            , trustAddResult: _notAsked
            , getBalanceResult: _notAsked
            , checkUBIPayoutResult: _notAsked
            , requestUBIPayoutResult: _notAsked
            , getUsersResult: _notAsked
            , transferResult: _notAsked
            , userSearchResult: _notAsked
            }
