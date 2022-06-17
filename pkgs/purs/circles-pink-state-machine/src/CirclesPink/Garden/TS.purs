module CirclesPink.Garden.TS where

import Prelude

import CirclesPink.Garden.Env as Garden
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Config as C
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)

type CirclesConfig =
  { extractEmail :: Either String (String -> Effect Unit)
  }

convertConfig :: forall m. MonadEffect m => CirclesConfig -> C.CirclesConfig m
convertConfig cfg = C.CirclesConfig
  { extractEmail: map (map liftEffect) $ cfg.extractEmail
  }

mkControl :: Garden.EnvVars -> CirclesConfig -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl envVars cfg setState s a =
  (circlesControl env (C.mapCirclesConfig liftEffect (convertConfig cfg)) (liftEffect <<< setState) s a)
    # launchAff_
  where
  request = milkisRequest windowFetch

  env = Garden.env { request, envVars }
