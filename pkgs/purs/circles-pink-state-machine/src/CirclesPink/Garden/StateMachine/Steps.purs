module CirclesPink.Garden.StateMachine.Steps
  ( askEmail
  , askUsername
  , infoGeneral
  , magicWords
  , submit
  ) where

import Prelude

import CirclesPink.Garden.Env (TestEnvM, liftEnv, runTestEnvM, testEnv)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.State (StateT, execStateT)
import Stadium.Control (toStateT)

type TestConfig = CirclesConfig (StateT CirclesState TestEnvM)

act :: TestConfig -> CirclesAction -> StateT CirclesState TestEnvM Unit
act cfg = toStateT $ circlesControl (liftEnv testEnv) cfg

execFrom :: CirclesState -> StateT CirclesState TestEnvM Unit -> CirclesState
execFrom st m = runTestEnvM $ execStateT m st

--------------------------------------------------------------------------------
-- Steps
--------------------------------------------------------------------------------
infoGeneral :: CirclesState
infoGeneral = S.init

askUsername :: TestConfig -> CirclesState
askUsername cfg =
  execFrom infoGeneral do
    act cfg $ A._infoGeneral $ A._next unit
    act cfg $ A._askUsername $ A._setUsername "fooo"

askEmail :: TestConfig -> CirclesState
askEmail cfg =
  execFrom (askUsername cfg) do
    act cfg $ A._askUsername $ A._next unit
    act cfg $ A._askEmail $ A._setEmail "helloworld@helloworld.com"
    act cfg $ A._askEmail $ A._setTerms unit
    act cfg $ A._askEmail $ A._setPrivacy unit

infoSecurity :: TestConfig -> CirclesState
infoSecurity cfg =
  execFrom (askEmail cfg) do
    act cfg $ A._askEmail $ A._next unit

magicWords :: TestConfig -> CirclesState
magicWords cfg =
  execFrom (infoSecurity cfg) do
    act cfg $ A._infoSecurity $ A._next unit

submit :: TestConfig -> CirclesState
submit cfg =
  execFrom (magicWords cfg) do
    act cfg $ A._magicWords $ A._next unit
