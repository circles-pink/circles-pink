module CirclesPink.Garden.TS where

import Prelude
import CirclesPink.Garden.Env as Garden
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Control.Monad.Identity.Trans (IdentityT, runIdentityT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)

circlesControlEff :: Env Aff -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
circlesControlEff e f s a =
  ((circlesControl e (liftEffect <<< f) s a) :: IdentityT Aff Unit)
    # runIdentityT
    # launchAff_

mkControl :: Garden.EnvVars -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl envVars =
  milkisRequest windowFetch
    # (\request -> Garden.env { request, envVars })
    # circlesControlEff
