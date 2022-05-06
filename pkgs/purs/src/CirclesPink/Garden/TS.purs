module CirclesPink.Garden.TS where

import Prelude
import CirclesPink.Garden.Env as Garden
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Control.Monad.Identity.Trans (IdentityT(..), runIdentityT)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import HTTP (addLogging)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)
import Stadium.Control (Control)
import Undefined (undefined)

type TM a
  = T M a

type T m a
  = IdentityT m a

type M
  = ReaderT (Env Aff) Aff

mkEnv :: Garden.EnvVars -> Env Aff
mkEnv envVars = Garden.env { request: milkisRequest windowFetch, envVars }

c :: Garden.EnvVars -> Control CirclesState CirclesAction TM
c envVars set st ac = undefined $ circlesControl (undefined :: Env M) (IdentityT $ ReaderT (\_ -> set)) st ac

-- circlesControlEff :: Env Aff -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
-- circlesControlEff e f s a =
--   ((circlesControl e (ReaderT (\_ -> liftEffect <<< f)) s a) :: IdentityT (ReaderT (Env Aff) Aff) Unit)
--     # runIdentityT
--     # launchAff_
-- mkControl :: Garden.EnvVars -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
-- mkControl envVars =
--   milkisRequest windowFetch
--     # addLogging
--     # (\request -> Garden.env { request, envVars })
--     # circlesControlEff
