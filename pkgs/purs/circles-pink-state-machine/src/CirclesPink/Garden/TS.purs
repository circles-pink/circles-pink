module CirclesPink.Garden.TS
  ( CirclesConfigEffect(..)
  , mkControl
  , mkControlTestEnv
  )
  where

import CirclesPink.Prelude

import CirclesPink.Garden.EnvControlAff as Garden
import CirclesPink.Garden.EnvControlTest (liftEnv, testEnv)
import CirclesPink.Garden.StateMachine (CirclesAction, CirclesConfig, CirclesState, circlesControl, initLanding)
import CirclesPink.Garden.StateMachine.Config as C
import CirclesPink.Garden.StateMachine.Control.Class.ProdM (runProdM)
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptT (evalTestScriptT)
import CirclesPink.Garden.StateMachine.TrackingEvent as TE
import CirclesPink.Garden.StateMachine.TrackingResumee as Tr
import Effect.Now (now)
import Effect.Unsafe (unsafePerformEffect)
import HTTP.Milkis (milkisRequest)
import Milkis.Impl (FetchImpl)
import StringStorage (getLocalStorage, getSessionStorage)

newtype CirclesConfigEffect = CirclesConfigEffect (CirclesConfig Effect)

mkControl :: FetchImpl -> Garden.EnvVars -> CirclesConfigEffect -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
mkControl fetch envVars (CirclesConfigEffect cfg) setState s a = do
  time <- now
  fold ((cfg -# _.onTrackingEvent) <*> TE.fromAction s a)
  fold ((cfg -# _.onTrackingResumee) <*> Tr.fromAction (wrap time) a)

  localStorage <- getLocalStorage
  sessionStorage <- getSessionStorage
  let
    env = liftEnv liftAff $ Garden.env
      { request
      , envVars
      , localStorage
      , sessionStorage
      , crypto:
          { encrypt: \_ str -> pure str
          , decrypt: \_ str -> pure $ Just str
          }
      , safeAddress: cfg -# _.safeAddress
      , strictMode: cfg -# _.strictMode
      }
  circlesControl env cfg' (liftEffect <<< handler) s a
    # runProdM cfg'
    # launchAff_
  where
  cfg' = C.mapCirclesConfig liftEffect cfg
  request = milkisRequest fetch
  handler f = do
    time <- now
    setState \statePrev ->
      let
        stateNext = f statePrev
        _ = unsafePerformEffect $ fold ((cfg -# _.onTrackingEvent) <*> TE.fromStateUpdate { prev: statePrev, next: stateNext })
        _ = unsafePerformEffect $ fold ((cfg -# _.onTrackingResumee) <*> Tr.fromStateUpdate (wrap time) { prev: statePrev, next: stateNext })
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
    , safeAddress: Nothing
    , strictMode: false
    }



instance ToPursNominal CirclesConfigEffect where
  toPursNominal _ = PursNominal "CirclesPink.Garden.TS" "CirclesConfigEffect"

instance ToTsType CirclesConfigEffect where
  toTsType = defaultToTsType' []

instance ToTsDef CirclesConfigEffect where
  toTsDef = defaultToTsDef' []

instance ToPursType CirclesConfigEffect where
  toPursType = defaultToPursType' []

