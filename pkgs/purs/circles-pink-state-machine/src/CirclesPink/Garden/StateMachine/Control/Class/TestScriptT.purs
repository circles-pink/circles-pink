module CirclesPink.Garden.StateMachine.Control.Class.TestScriptT
  ( TestScriptT
  , evalTestScriptT
  , execTestScriptT
  , runTestScriptT
  )
  where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (class MonadScript)
import Control.Monad.State (class MonadState, StateT, runStateT, state)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Log.Class (class MonadLog)

--------------------------------------------------------------------------------
newtype TestScriptT m a = TestScriptM (TestScriptT' m a)

type TestScriptT' m a = StateT CirclesState (StateT {} m) a

-- liftMonadTestEnv :: forall m a. MonadTestEnv m => (State {} a) -> TestScriptM a
-- liftMonadTestEnv = TestScriptM <<< lift

instance monadEffectScriptM :: MonadEffect m => MonadEffect (TestScriptT m) where
  liftEffect = liftEffect >>> TestScriptM

instance monadCirclesTestScriptT :: Monad m => MonadCircles (TestScriptT m) where
  sleep _ = pure unit
  -- getSafeAddress = todo

instance monadScriptTestScriptT :: Monad m => MonadScript (TestScriptT m)

instance monadLogTestScriptT :: Monad m => MonadLog (TestScriptT m) where
  log _ = pure unit

instance monadStateTestScriptT :: Monad m => MonadState CirclesState (TestScriptT m) where
  state = TestScriptM <<< state

-- Cannot derive because of '[1/1 PartiallyAppliedSynonym] (unknown module)' error
-- May be fixed in v15
unwrapTestScriptT :: forall m a. TestScriptT m a -> TestScriptT' m a
unwrapTestScriptT (TestScriptM x) = x

instance applyTestScriptT :: Monad m => Apply (TestScriptT m) where
  apply (TestScriptM f) (TestScriptM x) = TestScriptM $ apply f x

instance bindTestScriptT :: Monad m => Bind (TestScriptT m) where
  bind (TestScriptM x) f = TestScriptM $ bind x (f >>> unwrapTestScriptT)

instance applicativeTestScriptT :: Monad m => Applicative (TestScriptT m) where
  pure x = TestScriptM $ pure x

instance monadTestScriptT :: Monad m => Monad (TestScriptT m)

instance functorTestScriptT :: Functor m => Functor (TestScriptT m) where
  map f (TestScriptM x) = TestScriptM $ map f x

runTestScriptT :: forall m a. Monad m => CirclesState -> TestScriptT m a -> m (a /\ CirclesState /\ {})
runTestScriptT st (TestScriptM x) = runStateT x st
  # (\y -> runStateT y {})
  <#> (\((a /\ cs) /\ ts) -> a /\ cs /\ ts)

execTestScriptT :: forall m a. Monad m => CirclesState -> TestScriptT m a -> m (CirclesState /\ {})
execTestScriptT st x = runTestScriptT st x <#> snd

evalTestScriptT :: forall m a. Monad m => CirclesState -> TestScriptT m a -> m a
evalTestScriptT st x = runTestScriptT st x <#> fst