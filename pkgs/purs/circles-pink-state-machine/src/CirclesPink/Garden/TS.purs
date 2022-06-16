module CirclesPink.Garden.TS where

import Prelude

import CirclesPink.Garden.Env as Garden
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig, mapCirclesConfig)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)

mkControl :: Garden.EnvVars -> CirclesConfig Effect -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl envVars cfg setState s a =
  (circlesControl env (mapCirclesConfig liftEffect cfg) (liftEffect <<< setState) s a)
    # launchAff_
  where
  request = milkisRequest windowFetch

  env = Garden.env { request, envVars }
