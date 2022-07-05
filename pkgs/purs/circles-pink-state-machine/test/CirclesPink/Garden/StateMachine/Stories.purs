module Test.CirclesPink.Garden.StateMachine.Stories
  ( spec
  ) where

import Prelude

import CirclesPink.Data.Mnemonic (sampleMnemonic)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState, initLanding)
import CirclesPink.Garden.StateMachine.Stories (class MonadScript, finalizeAccount, loginUser, signUpUser)
import CirclesPink.Garden.TestEnv (TestEnvM, liftEnv, runTestEnvM, testEnv)
import Control.Monad.State (class MonadState, StateT, lift, runStateT, state)
import Data.Either (Either(..))
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Data.Variant.Extra (getLabel)
import Log.Class (class MonadLog)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

--------------------------------------------------------------------------------
spec :: Spec Unit
spec =
  let
    env' :: Env TestScriptM
    env' = liftEnv (TestScriptM <<< lift) testEnv

    cfg = CirclesConfig { extractEmail: Right (\_ -> pure unit) }
  in
    describe "CirclesPink.Garden.StateMachine.Stories" do
      describe "A user can signup" do
        it "ends up in `trusts` state" do
          ( signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
              # execTestScriptM
              # getLabel
          )
            `shouldEqual`
              "trusts"
      describe "A user can finalize the account" do
        it "ends up in `dashboard` state" do
          ( ( do
                _ <- signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
                _ <- finalizeAccount env' cfg
                pure unit
            )
              # execTestScriptM
              # getLabel
          )
            `shouldEqual`
              "dashboard"
      describe "A user can not login with invalid mnemonic" do
        it "stays in `login` state" do
          ( ( do
                _ <- loginUser env' cfg { magicWords: "" }
                pure unit
            )
              # execTestScriptM
              # getLabel
          )
            `shouldEqual`
              "login"
      describe "A user can not login with unregistered account" do
        it "stays in `login` state" do
          ( ( do
                _ <- loginUser env' cfg { magicWords: "volcano agree attack fiction firm chunk sweet private average undo pen core plunge choose vendor way liar depth romance enjoy hire rhythm little later" }
                pure unit
            )
              # execTestScriptM
              # getLabel
          )
            `shouldEqual`
              "login"
      describe "A user can login" do
        it "ends up in `dashboard` state" do
          ( ( do
                _ <- loginUser env' cfg { magicWords: show sampleMnemonic }
                pure unit
            )
              # execTestScriptM
              # getLabel
          )
            `shouldEqual`
              "dashboard"

-- -- describe "A user can trust" do
-- --   it "can trust another user" do
-- --     ( ( do
-- --           _ <- loginUser env' { magicWords: show sampleMnemonic }
-- --           _ <- trustUser env' { safeAddress: addrToString sampleSafeAddress }
-- --           pure unit
-- --       )
-- --         # (\x -> spy "b" x)
-- --         # execScripT'
-- --         # runTestEnvM
-- --         # (\x -> spy "a" x)
-- --         #
-- --           ( default Nothing
-- --               # onMatch
-- --                   { dashboard: \st ->
-- --                       let
-- --                         x = spy "x" (addrToString sampleSafeAddress)
-- --                         y = spy "y" $ (toUnfoldable st.trusts :: Array _)
-- --                       in
-- --                         lookup (convert sampleSafeAddress) st.trusts
-- --                           <#> (const unit)
-- --                   }
-- --           )
-- --     )
-- --       `shouldEqual` (Just unit)


--------------------------------------------------------------------------------
newtype TestScriptM a = TestScriptM (StateT CirclesState TestEnvM a)

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

runTestScriptM :: forall a. TestScriptM a -> a /\ CirclesState
runTestScriptM (TestScriptM x) = runStateT x initLanding # runTestEnvM

execTestScriptM :: forall a. TestScriptM a -> CirclesState
execTestScriptM = runTestScriptM >>> snd