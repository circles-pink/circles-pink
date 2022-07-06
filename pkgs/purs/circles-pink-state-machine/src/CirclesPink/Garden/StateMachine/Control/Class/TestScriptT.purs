module CirclesPink.Garden.StateMachine.Control.Class.TestScriptT
  ( TestScriptT
  , evalTestScriptT
  , execTestScriptT
  , runTestScriptT
  ) where

import Prelude

import CirclesPink.Garden.StateMachine (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (class MonadScript)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, runStateT, state)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Log.Class (class MonadLog)

--------------------------------------------------------------------------------
newtype TestScriptT m a = TestScriptT (TestScriptT' m a)

type TestScriptT' m a =
  ReaderT (CirclesConfig (TestScriptT m))
    ( StateT CirclesState
        (StateT {} m)
    )
    a

-- liftMonadTestEnv :: forall m a. MonadTestEnv m => (State {} a) -> TestScriptT a
-- liftMonadTestEnv = TestScriptT <<< lift

instance monadEffectScriptM :: MonadEffect m => MonadEffect (TestScriptT m) where
  liftEffect = liftEffect >>> TestScriptT

instance monadCirclesTestScriptT :: Monad m => MonadCircles (TestScriptT m) where
  sleep _ = pure unit

instance monadAsk :: Monad m => MonadAsk (CirclesConfig (TestScriptT m)) (TestScriptT m) where
  ask = TestScriptT $ ask

-- getSafeAddress = todo

instance monadScriptTestScriptT :: Monad m => MonadScript (TestScriptT m)

instance monadLogTestScriptT :: Monad m => MonadLog (TestScriptT m) where
  log _ = pure unit

instance monadStateTestScriptT :: Monad m => MonadState CirclesState (TestScriptT m) where
  state = TestScriptT <<< state

-- Cannot derive because of '[1/1 PartiallyAppliedSynonym] (unknown module)' error
-- May be fixed in v15
unwrapTestScriptT :: forall m a. TestScriptT m a -> TestScriptT' m a
unwrapTestScriptT (TestScriptT x) = x

instance applyTestScriptT :: Monad m => Apply (TestScriptT m) where
  apply (TestScriptT f) (TestScriptT x) = TestScriptT $ apply f x

instance bindTestScriptT :: Monad m => Bind (TestScriptT m) where
  bind (TestScriptT x) f = TestScriptT $ bind x (f >>> unwrapTestScriptT)

instance applicativeTestScriptT :: Monad m => Applicative (TestScriptT m) where
  pure x = TestScriptT $ pure x

instance monadTestScriptT :: Monad m => Monad (TestScriptT m)

instance functorTestScriptT :: Functor m => Functor (TestScriptT m) where
  map f (TestScriptT x) = TestScriptT $ map f x

runTestScriptT :: forall m a. Monad m => CirclesConfig (TestScriptT m) ->  CirclesState -> TestScriptT m a -> m (a /\ CirclesState /\ {})
runTestScriptT cfg st (TestScriptT x) = x
  # flip runReaderT cfg
  # flip runStateT st
  # flip runStateT {}
  <#> (\((a /\ cs) /\ ts) -> a /\ cs /\ ts)

execTestScriptT :: forall m a. Monad m => CirclesConfig (TestScriptT m) -> CirclesState -> TestScriptT m a -> m (CirclesState /\ {})
execTestScriptT cfg st x = runTestScriptT cfg st x <#> snd

evalTestScriptT :: forall m a. Monad m => CirclesConfig (TestScriptT m) -> CirclesState -> TestScriptT m a -> m a
evalTestScriptT cfg st x = runTestScriptT cfg st x <#> fst