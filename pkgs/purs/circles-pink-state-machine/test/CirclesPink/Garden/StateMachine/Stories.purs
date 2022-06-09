module Test.CirclesPink.Garden.StateMachine.Stories
  ( spec
  ) where

import Prelude

import CirclesPink.Garden.Env (TestEnvM, liftEnv, runTestEnvM, testEnv)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (ScriptT, finalizeAccount, loginUser, runScripT, signUpUser)
import Control.Monad.State (StateT, runState)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.String as S
import Data.Tuple (snd)
import Data.Variant.Extra (getLabel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Wallet.PrivateKey (sampleMnemonic)

--------------------------------------------------------------------------------

type AppM e a = ScriptT e TestEnvM a

execAppM :: forall e a. AppM e a -> CirclesState
execAppM x = x # runScripT # runTestEnvM # snd

--------------------------------------------------------------------------------

spec :: Spec Unit
spec =
  let
    env' :: Env (StateT CirclesState TestEnvM)
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
      describe "A user can not login with wrong credentials" do
        it "ends up in `dashboard` state" do
          ( ( do
                _ <- loginUser env' { magicWords: "" }
                pure unit
            )
              # execAppM
              # getLabel
          )
            `shouldEqual` "dashboard"
      describe "A user can login" do
        it "ends up in `dashboard` state" do
          ( ( do
                _ <- loginUser env' { magicWords: show sampleMnemonic }
                pure unit
            )
              # execAppM
              # getLabel
          )
            `shouldEqual` "dashboard"
