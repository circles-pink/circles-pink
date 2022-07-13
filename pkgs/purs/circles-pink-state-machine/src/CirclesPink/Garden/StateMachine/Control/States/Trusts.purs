module CirclesPink.Garden.StateMachine.Control.States.Trusts
  ( trusts
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', deploySafe', dropError, readyForDeployment, retryUntil, runExceptT', subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (lift, runExceptT)
import Data.Either (Either(..), isRight)
import RemoteData (_failure)

trusts
  :: forall m
   . Monad m
  => Env.Env m
  -> { getSafeStatus :: ActionHandler' m Unit S.TrustState ("trusts" :: S.TrustState)
     , finalizeRegisterUser :: ActionHandler' m Unit S.TrustState ("trusts" :: S.TrustState, "dashboard" :: S.DashboardState)
     }
trusts env@{ deployToken } =
  { getSafeStatus
  , finalizeRegisterUser
  }
  where
  getSafeStatus set st _ = do
    let
      task = do
        safeStatus <- env.getSafeStatus st.privKey
        isReady' <- readyForDeployment env st.privKey
        trusts' <- env.trustGetNetwork st.privKey st.user.safeAddress
        pure { safeStatus, isReady: isReady', trusts: trusts' }
    results <- runExceptT' task
    case results of
      Left e -> set \st' -> S._trusts st' { trustsResult = _failure { error: e, timestamp: bottom, retry: 0 } }
      Right r -> set \st' -> S._trusts st' { safeStatus = r.safeStatus, isReady = r.isReady, trusts = r.trusts }

  finalizeRegisterUser set st _ =
    let
      doDeploys =
        do
          _ <- deploySafe' env st.privKey
            # subscribeRemoteReport env (\r -> set \st' -> S._trusts st' { deploySafeResult = r })
            # retryUntil env (const { delay: 250 })
                ( \r _ -> case r of
                    Right res -> res.isCreated && res.isDeployed
                    Left _ -> false
                )
                0
            # dropError
          _ <- deployToken st.privKey
            # subscribeRemoteReport env (\r -> set \st' -> S._trusts st' { deployTokenResult = r })
            # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0
            # dropError
          pure unit

    in
      void do
        runExceptT do
          _ <- doDeploys
          lift $ set \_ -> S.initDashboard
            { user: st.user, privKey: st.privKey }

