module CirclesPink.Garden.TS
  ( convertConfig
  , mkControl
  , mkControlTestEnv
  ) where

import Prelude

import CirclesPink.Garden.Env as Garden
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Config as C
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptT (evalTestScriptT)
import CirclesPink.Garden.StateMachine.State (CirclesState, initLanding)
import CirclesPink.Garden.TestEnv (testEnv)
import Data.Either (Either(..))
import Data.FpTs.Either as FP
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import FpTs.Class (fromFpTs)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)

type CirclesConfig =
  { extractEmail :: FP.Either String (String -> Effect Unit)
  }

convertConfig :: forall m. MonadEffect m => CirclesConfig -> C.CirclesConfig m
convertConfig cfg = C.CirclesConfig
  { extractEmail: map (map liftEffect) $ fromFpTs $ cfg.extractEmail
  }

mkControl :: Garden.EnvVars -> CirclesConfig -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl envVars cfg setState s a =
  (circlesControl env (C.mapCirclesConfig liftEffect (convertConfig cfg)) (liftEffect <<< setState) s a)
    # launchAff_
  where
  request = milkisRequest windowFetch

  env = Garden.env { request, envVars }

mkControlTestEnv :: ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControlTestEnv setState st ac = 
  circlesControl testEnv cfg (setState >>> liftEffect) st ac
    # evalTestScriptT initLanding
  where
  cfg = C.CirclesConfig
    { extractEmail:
        --Left "my@email.com"
        Right (\_ -> pure unit)
    }