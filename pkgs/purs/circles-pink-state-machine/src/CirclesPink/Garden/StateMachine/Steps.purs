module CirclesPink.Garden.StateMachine.Steps
  ( askEmail
  , askUsername
  , infoGeneral
  , infoSecurity
  , magicWords
  , submit
  )
  where

import Prelude

import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptM (TestScriptM, execTestScriptM, liftTestEnvM)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.TestEnv (liftEnv, testEnv)
import Data.Either (Either(..))
import Stadium.Control (toMonadState)

testConfig :: forall m. Monad m => CirclesConfig m
testConfig = CirclesConfig
  { extractEmail: Right $ const $ pure unit
  }

act :: CirclesAction -> TestScriptM Unit
act ac = (toMonadState $ circlesControl (liftEnv liftTestEnvM testEnv) testConfig) ac

--------------------------------------------------------------------------------
-- Steps
--------------------------------------------------------------------------------
infoGeneral :: CirclesState
infoGeneral = S.init

askUsername :: CirclesState
askUsername =
  execTestScriptM infoGeneral do
    act $ A._infoGeneral $ A._next unit
    act $ A._askUsername $ A._setUsername "fooo"

askEmail :: CirclesState
askEmail =
  execTestScriptM askUsername do
    act $ A._askUsername $ A._next unit
    act $ A._askEmail $ A._setEmail "helloworld@helloworld.com"
    act $ A._askEmail $ A._setTerms unit
    act $ A._askEmail $ A._setPrivacy unit

infoSecurity :: CirclesState
infoSecurity =
  execTestScriptM askEmail do
    act $ A._askEmail $ A._next unit

magicWords :: CirclesState
magicWords =
  execTestScriptM infoSecurity do
    act $ A._infoSecurity $ A._next unit

submit :: CirclesState
submit =
  execTestScriptM magicWords do
    act $ A._magicWords $ A._next unit
