module CirclesPink.Garden.StateMachine.Control.States.Dashboard where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Either (Either(..))
import Undefined (undefined)
import Wallet.PrivateKey as P

dashboard ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { logout :: ActionHandler t m Unit S.DashboardState ( "landing" :: S.LandingState )
  , getTrusts :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  , addTrustConnection :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  }
dashboard env =
  { logout: \_ _ _ -> pure unit
  , getTrusts
  , addTrustConnection
  }
  where
  getTrusts set st _ = do
    result <-
      run $ env.trustGetNetwork st.privKey
    case result of
      Left e -> set \st' -> S._dashboard st' { error = pure e }
      Right t -> set \st' -> S._dashboard st' { trusts = t }

  addTrustConnection set st u = do
    -- result <- run $ env.addTrustConnection st.privKey (P.unsafeAddrFromString u) st.user.safeAddress
    pure unit
