module CirclesPink.Garden.StateMachine.Steps
  -- ( askEmail
  -- , askUsername
  -- , infoGeneral
  -- , infoSecurity
  -- , magicWords
  -- , submit
  -- )
   where

import Prelude

import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.TestEnv (TestEnvM, liftEnv, runTestEnvM, testEnv)
import Control.Monad.State (StateT, execStateT)
import Debug.Extra (todo)
import Stadium.Control (toStateT)

-- type TestConfig = CirclesConfig  (StateT CirclesState TestEnvM)

-- newtype StepM a = StepM (StateT CirclesState TestEnvM a)

-- derive newtype instance monad :: Monad StepM 

-- instance monadCircles :: MonadCircles StepM where
--   sleep _ = pure unit

-- act :: TestConfig -> CirclesAction -> StepM Unit
-- act cfg ac = StepM $ (toStateT $ circlesControl (liftEnv testEnv) cfg) ac

-- execFrom :: CirclesState -> StepM Unit -> CirclesState
-- execFrom st (StepM m) = runTestEnvM $ execStateT m st

-- --------------------------------------------------------------------------------
-- -- Steps
-- --------------------------------------------------------------------------------
-- infoGeneral :: CirclesState
-- infoGeneral = S.init

-- askUsername :: TestConfig -> CirclesState
-- askUsername cfg =
--   execFrom infoGeneral do
--     act cfg $ A._infoGeneral $ A._next unit
--     act cfg $ A._askUsername $ A._setUsername "fooo"

-- askEmail :: TestConfig -> CirclesState
-- askEmail cfg =
--   execFrom (askUsername cfg) do
--     act cfg $ A._askUsername $ A._next unit
--     act cfg $ A._askEmail $ A._setEmail "helloworld@helloworld.com"
--     act cfg $ A._askEmail $ A._setTerms unit
--     act cfg $ A._askEmail $ A._setPrivacy unit

-- infoSecurity :: TestConfig -> CirclesState
-- infoSecurity cfg =
--   execFrom (askEmail cfg) do
--     act cfg $ A._askEmail $ A._next unit

-- magicWords :: TestConfig -> CirclesState
-- magicWords cfg =
--   execFrom (infoSecurity cfg) do
--     act cfg $ A._infoSecurity $ A._next unit

-- submit :: TestConfig -> CirclesState
-- submit cfg =
--   execFrom (magicWords cfg) do
--     act cfg $ A._magicWords $ A._next unit
