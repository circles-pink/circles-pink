module CirclesPink.Garden.StateMachine.Control.States.Trusts
  ( trusts
  ) where

import Prelude

import CirclesCore (ErrSafeGetSafeStatus, SafeStatus)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', dropError, readyForDeployment, retryUntil, runExceptT', subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..), isRight)
import Debug.Extra (todo)
import RemoteData (_failure)
import Type.Row (type (+))
import Wallet.PrivateKey (PrivateKey)

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
      doDeploys =
        do
          _ <- deploySafe' env st.privKey
            # subscribeRemoteReport env (\r -> set \st' -> S._trusts st' { deploySafeResult = r })
            # retryUntil env (const { delay: 250 }) (\r _ -> isRight r) 0
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

--------------------------------------------------------------------------------

type ErrDeploySafe' r = Env.ErrDeploySafe + Env.ErrGetSafeStatus + r

deploySafe' :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrDeploySafe' r) m SafeStatus
deploySafe' { deploySafe, getSafeStatus } privKey = do
  _ <- deploySafe privKey
  safeStatus <- getSafeStatus privKey
  pure safeStatus