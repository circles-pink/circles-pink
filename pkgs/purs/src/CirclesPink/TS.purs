module CirclesPink.TS where

import Prelude
import CirclesPink.StateMachine.Action (CirclesAction)
import CirclesPink.StateMachine.Control (Env, circlesControl)
import CirclesPink.StateMachine.State (CirclesState)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Garden.Env as Garden
import HTTP.Milkis (milkisRequest)
import Milkis.Impl.Window (windowFetch)

circlesControlEff :: Env Aff -> ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
circlesControlEff e f s a = circlesControl e (liftEffect <<< f) s a # launchAff_

control :: ((CirclesState -> CirclesState) -> Effect Unit) -> CirclesState -> CirclesAction -> Effect Unit
control =
  milkisRequest windowFetch
    # (\request -> Garden.env { request })
    # circlesControlEff
