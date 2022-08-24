module CirclesPink.Garden.TS
  ( CirclesConfig
  , convertConfig
  , mkControl
  , mkControlTestEnv
  ) where

import CirclesPink.Prelude

import CirclesPink.Garden.EnvControlAff as Garden
import CirclesPink.Garden.EnvControlTest (liftEnv, testEnv)
import CirclesPink.Garden.StateMachine (CirclesAction, circlesControl, CirclesState, initLanding)
import CirclesPink.Garden.StateMachine.Config as C
import CirclesPink.Garden.StateMachine.Control.Class.ProdM (runProdM)
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptT (evalTestScriptT)
import CirclesPink.Garden.StateMachine.TrackingEvent (TrackingEvent)
import CirclesPink.Garden.StateMachine.TrackingEvent as TE
import CirclesPink.Garden.StateMachine.TrackingResumee (Resumee)
import CirclesPink.Garden.StateMachine.TrackingResumee as Tr
import Data.FpTs.Either as FP
import Effect.Now (now)
import Effect.Unsafe (unsafePerformEffect)
import FpTs.Class (fromFpTs)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)
import StringStorage (getLocalStorage, getSessionStorage, testStringStorage)

type CirclesConfig =
  { extractEmail :: FP.Either String (String -> Effect Unit)
  , onTrackingEvent :: Maybe (TrackingEvent -> Effect Unit)
  , onTrackingResumee :: Maybe ((Resumee -> Resumee) -> Effect Unit)
  }

convertConfig :: forall m. MonadEffect m => CirclesConfig -> C.CirclesConfig m
convertConfig cfg = C.CirclesConfig
  { extractEmail: map (map liftEffect) $ fromFpTs $ cfg.extractEmail
  , onTrackingEvent: map (map liftEffect) $ cfg.onTrackingEvent
  , onTrackingResumee: map (map liftEffect) $ cfg.onTrackingResumee
  }

mkControl :: Garden.EnvVars -> CirclesConfig -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl envVars cfg setState s a = do
  time <- now
  fold (cfg.onTrackingEvent <*> TE.fromAction s a)
  fold (cfg.onTrackingResumee <*> Tr.fromAction (wrap time) a)

  localStorage <- getLocalStorage
  sessionStorage <- getSessionStorage
  let
    env = liftEnv liftAff $ Garden.env
      { request
      , envVars
      , localStorage
      , sessionStorage
      , crypto:
          { encrypt: \_ str -> str
          , decrypt: \_ str -> Just str
          }
      }
  circlesControl env cfg' (liftEffect <<< handler) s a
    # runProdM cfg'
    # launchAff_
  where
  cfg' = C.mapCirclesConfig liftEffect $ convertConfig cfg
  request = milkisRequest windowFetch
  handler f = do
    time <- now
    setState \statePrev ->
      let
        stateNext = f statePrev
        _ = unsafePerformEffect $ fold (cfg.onTrackingEvent <*> TE.fromStateUpdate { prev: statePrev, next: stateNext })
        _ = unsafePerformEffect $ fold (cfg.onTrackingResumee <*> Tr.fromStateUpdate (wrap time) { prev: statePrev, next: stateNext })
      in
        stateNext

mkControlTestEnv :: ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControlTestEnv setState st ac =
  circlesControl testEnv cfg (setState >>> liftEffect) st ac
    # evalTestScriptT cfg initLanding
  where
  cfg = C.CirclesConfig
    { extractEmail: Right (\_ -> pure unit)
    , onTrackingEvent: Nothing
    , onTrackingResumee: Nothing
    }