module CirclesPink.Garden.StateMachine.Control.Class.TestScriptM
  ( TestScriptM
  , evalTestScriptM
  , execTestScriptM
  , liftTestEnvM
  ) where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (class MonadScript)
import CirclesPink.Garden.TestEnv (TestEnvM, runTestEnvM)
import Control.Monad.State (class MonadState, StateT, lift, runStateT, state)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Log.Class (class MonadLog)

--------------------------------------------------------------------------------
newtype TestScriptM a = TestScriptM (StateT CirclesState TestEnvM a)

liftTestEnvM :: forall a. TestEnvM a -> TestScriptM a
liftTestEnvM = TestScriptM <<< lift

instance monadCirclesTestScriptM :: MonadCircles TestScriptM where
  sleep _ = pure unit

instance monadScriptTestScriptM :: MonadScript TestScriptM

instance monadLogTestScriptM :: MonadLog TestScriptM where
  log _ = pure unit

instance monadStateTestScriptM :: MonadState CirclesState TestScriptM where
  state = TestScriptM <<< state

-- Cannot derive because of '[1/1 PartiallyAppliedSynonym] (unknown module)' error
-- May be fixed in v15
unwrapTestScriptM :: forall a. TestScriptM a -> (StateT CirclesState TestEnvM a)
unwrapTestScriptM (TestScriptM x) = x

instance applyTestScriptM :: Apply TestScriptM where
  apply (TestScriptM f) (TestScriptM x) = TestScriptM $ apply f x

instance bindTestScriptM :: Bind TestScriptM where
  bind (TestScriptM x) f = TestScriptM $ bind x (f >>> unwrapTestScriptM)

instance applicativeTestScriptM :: Applicative TestScriptM where
  pure x = TestScriptM $ pure x

instance monadTestScriptM :: Monad TestScriptM

instance functorTestScriptM :: Functor TestScriptM where
  map f (TestScriptM x) = TestScriptM $ map f x

runTestScriptM :: forall a. CirclesState -> TestScriptM a -> a /\ CirclesState
runTestScriptM st (TestScriptM x) = runStateT x st # runTestEnvM

execTestScriptM :: forall a. CirclesState -> TestScriptM a -> CirclesState
execTestScriptM st = runTestScriptM st >>> snd

evalTestScriptM :: forall a. CirclesState -> TestScriptM a -> a
evalTestScriptM st = runTestScriptM st >>> fst