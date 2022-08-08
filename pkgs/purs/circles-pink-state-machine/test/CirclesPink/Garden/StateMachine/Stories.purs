module Test.CirclesPink.Garden.StateMachine.Stories
  ( spec
  ) where

import Prelude

import CirclesPink.Data.Mnemonic (sampleMnemonic)
import CirclesPink.Garden.EnvControlTest (testEnv)
import CirclesPink.Garden.StateMachine (initLanding)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptT (TestScriptT, execTestScriptT)
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Stories (finalizeAccount, loginUser, signUpUser)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Data.Variant.Extra (getLabel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

--------------------------------------------------------------------------------
spec :: Spec Unit
spec =
  let
    env' :: EnvControl (TestScriptT Identity)
    env' = testEnv

    cfg = CirclesConfig { extractEmail: Right (\_ -> pure unit), onTrackingEvent : Nothing }

    execTestScriptM_ = execTestScriptT cfg initLanding >>> unwrap >>> fst
  in
    describe "CirclesPink.Garden.StateMachine.Stories" do
      describe "A user can signup" do
        it "ends up in `trusts` state" do
          ( signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
              # execTestScriptM_
              # getLabel
          )
            `shouldEqual`
              "trusts"
      describe "A user can finalize the account" do
        it "ends up in `dashboard` state" do
          ( ( do
                signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
                finalizeAccount env' cfg
                pure unit
            )
              # execTestScriptM_
              # getLabel
          )
            `shouldEqual`
              "dashboard"
      describe "A user can not login with invalid mnemonic" do
        it "stays in `login` state" do
          ( ( do
                loginUser env' cfg { magicWords: "" }
                pure unit
            )
              # execTestScriptM_
              # getLabel
          )
            `shouldEqual`
              "login"
      describe "A user can not login with unregistered account" do
        it "stays in `login` state" do
          ( ( do
                loginUser env' cfg { magicWords: "volcano agree attack fiction firm chunk sweet private average undo pen core plunge choose vendor way liar depth romance enjoy hire rhythm little later" }
                pure unit
            )
              # execTestScriptM_
              # getLabel
          )
            `shouldEqual`
              "login"
      describe "A user can login" do
        it "ends up in `dashboard` state" do
          ( ( do
                loginUser env' cfg { magicWords: show sampleMnemonic }
                pure unit
            )
              # execTestScriptM_
              # getLabel
          )
            `shouldEqual`
              "dashboard"

-- describe "A user can trust" do
--   it "can trust another user" do
--     ( ( do
--           _ <- loginUser env' { magicWords: show sampleMnemonic }
--           _ <- trustUser env' { safeAddress: addrToString sampleSafeAddress }
--           pure unit
--       )
--         # (\x -> spy "b" x)
--         # execScripT'
--         # runTestEnvM
--         # (\x -> spy "a" x)
--         #
--           ( default Nothing
--               # onMatch
--                   { dashboard: \st ->
--                       let
--                         x = spy "x" (addrToString sampleSafeAddress)
--                         y = spy "y" $ (toUnfoldable st.trusts :: Array _)
--                       in
--                         lookup (sampleSafeAddress) st.trusts
--                           <#> (const unit)
--                   }
--           )
--     )
--       `shouldEqual` (Just unit)
