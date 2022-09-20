module CirclesPink.Garden.StateMachine.Control.Class.ScriptM
  ( ScriptM
  , evalScriptM
  , execScriptM
  , runScriptM
  ) where

import Prelude

import CirclesPink.Garden.StateMachine (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.State (CirclesState, initLanding)
import CirclesPink.Garden.StateMachine.Stories (class MonadScript)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, runStateT, state)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as E
import Log.Class (class MonadLog)

type ScriptM' a =
  ReaderT (CirclesConfig ScriptM)
    ( StateT CirclesState
        (StateT {} Aff)
    )
    a

newtype ScriptM a = ScriptM (ScriptM' a)

instance MonadCircles ScriptM where
  sleep _ = pure unit

-- getSafeAddress x1 = getSafeAddress x1 # mapExceptT (ScriptM <<< liftAff)

instance MonadAsk (CirclesConfig ScriptM) ScriptM where
  ask = ScriptM $ ask

instance MonadScript ScriptM

instance MonadLog ScriptM where
  log = E.log >>> ScriptM

instance MonadState CirclesState ScriptM where
  state = ScriptM <<< state

instance MonadEffect ScriptM where
  liftEffect = liftEffect >>> ScriptM

instance MonadAff ScriptM where
  liftAff = liftAff >>> ScriptM

-- Cannot derive because of '[1/1 PartiallyAppliedSynonym] (unknown module)' error
-- May be fixed in v15
unwrapScriptM :: forall a. ScriptM a -> ScriptM' a
unwrapScriptM (ScriptM x) = x

instance Apply ScriptM where
  apply (ScriptM f) (ScriptM x) = ScriptM $ apply f x

instance Bind ScriptM where
  bind (ScriptM x) f = ScriptM $ bind x (f >>> unwrapScriptM)

instance Applicative ScriptM where
  pure x = ScriptM $ pure x

instance Monad ScriptM

instance Functor ScriptM where
  map f (ScriptM x) = ScriptM $ map f x

runScriptM :: forall a. CirclesConfig ScriptM -> ScriptM a -> Aff (a /\ CirclesState /\ {})
runScriptM cfg (ScriptM x) = x
  # flip runReaderT cfg
  # flip runStateT initLanding
  # flip runStateT {}
  <#> (\((a /\ cs) /\ ts) -> a /\ cs /\ ts)

execScriptM :: forall a. CirclesConfig ScriptM -> ScriptM a -> Aff (CirclesState /\ {})
execScriptM cfg x = runScriptM cfg x <#> snd

evalScriptM :: forall a. CirclesConfig ScriptM -> ScriptM a -> Aff a
evalScriptM cfg = runScriptM cfg >>> map fst