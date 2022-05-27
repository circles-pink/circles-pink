module CirclesPink.Garden.StateMachine.Control.States.Trusts
  ( trusts
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', dropError, readyForDeployment, retryUntil, runExceptT', subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (catchError, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..), isRight)
import RemoteData (_failure, _loading)
import Undefined (undefined)

trusts
  :: forall m
   . Monad m
  => Env.Env m
  -> { getSafeStatus :: ActionHandler' m Unit S.TrustState ("trusts" :: S.TrustState)
     , finalizeRegisterUser :: ActionHandler' m Unit S.TrustState ("trusts" :: S.TrustState, "dashboard" :: S.DashboardState)
     }
trusts env@{ deployToken, deploySafe } =
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
      Left e -> set \st' -> S._trusts st' { trustsResult = _failure { error: e, timestamp: bottom, retry: 0 } }
      Right r -> set \st' -> S._trusts st' { safeStatus = r.safeStatus, isReady = r.isReady, trusts = r.trusts }

  finalizeRegisterUser set st _ =
    let
      doDeploys = do
        _ <- (\_ -> deploySafe st.privKey)
          # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        _ <- (\_ -> deployToken st.privKey)
          # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
        pure unit
    in
      void do
        runExceptT do
          _ <- doDeploys
            # subscribeRemoteReport env (\r -> set \st' -> S._trusts st' { trustsResult = r })
            # (\x -> x 0)
            # dropError
          lift $ set \_ -> S.initDashboard
            { user: st.user, privKey: st.privKey }