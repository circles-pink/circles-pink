module CirclesPink.Garden.StateMachine.Steps
  ( askEmail
  , askUsername
  , infoSecurity
  , magicWords
  , submit
  ) where

import Prelude

import CirclesPink.Garden.EnvControlTest (testEnv)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Action as A
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control (circlesControl)
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptT (TestScriptT, execTestScriptT)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.State as S
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Stadium.Control (toMonadState)

testConfig :: forall m. Monad m => CirclesConfig m
testConfig = CirclesConfig
  { extractEmail: Right $ const $ pure unit
  , onTrackingEvent: Nothing
  }

act :: CirclesAction -> (TestScriptT Identity) Unit
act ac = (toMonadState $ circlesControl testEnv testConfig) ac

--------------------------------------------------------------------------------
-- Steps
--------------------------------------------------------------------------------

execTestScriptM :: forall a. CirclesState -> TestScriptT Identity a -> CirclesState
execTestScriptM x1 x2 = execTestScriptT testConfig x1 x2 <#> fst # unwrap

askUsername :: CirclesState
askUsername =
  execTestScriptM S.init do
    act $ A._askUsername $ A._next unit
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
