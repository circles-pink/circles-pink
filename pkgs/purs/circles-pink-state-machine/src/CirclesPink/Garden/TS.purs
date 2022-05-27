module CirclesPink.Garden.TS where

import Prelude
import CirclesPink.Garden.Env as Garden
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)

mkControl :: Garden.EnvVars -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl envVars f s a =
  (circlesControl env (liftEffect <<< f) s a)
    # launchAff_
  where
  request = milkisRequest windowFetch
  env = Garden.env { request, envVars }

