module CirclesPink.Garden.StateMachine.Stories.Tests
  ( spec
  ) where

import Prelude

import CirclesPink.Garden.Env (liftEnv, testEnv)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (ScriptT, finalizeAccount, runScripT, signUpUser)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Data.Variant.Extra (getLabel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

--------------------------------------------------------------------------------

type AppM e a = ScriptT e Identity a

execAppM :: forall e a. AppM e a -> CirclesState
execAppM x = x # runScripT # unwrap # snd

--------------------------------------------------------------------------------

spec :: Spec Unit
spec =
  let
    env' :: Env (StateT CirclesState Identity)
    env' = liftEnv testEnv
  in
    describe "CirclesPink.Garden.StateMachine.Stories" do
      describe "A user can signup" do
        it "ends up in `trusts` state" do
          ( signUpUser env' { username: "Foo", email: "foo@bar.com" }
              # execAppM
              # getLabel
          )
            `shouldEqual` "trusts"
      describe "A user can finalize the account" do
        it "ends up in `dashboard` state" do
          ( ( do
                _ <- signUpUser env' { username: "Foo", email: "foo@bar.com" }
                _ <- finalizeAccount env'
                pure unit
            )
              # execAppM
              # getLabel
          )
            `shouldEqual` "dashboard"
