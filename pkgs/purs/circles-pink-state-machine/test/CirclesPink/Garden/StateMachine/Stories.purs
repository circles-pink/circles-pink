module Test.CirclesPink.Garden.StateMachine.Stories
  ( spec
  ) where

import Prelude

import CirclesPink.Garden.Env (TestEnvM, liftEnv, runTestEnvM, testEnv)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (execScripT', finalizeAccount, loginUser, signUpUser)
import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Variant.Extra (getLabel)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Wallet.PrivateKey (sampleMnemonic)

--------------------------------------------------------------------------------
spec :: Spec Unit
spec =
  let
    env' :: Env (StateT CirclesState TestEnvM)
    env' = liftEnv testEnv

    cfg = CirclesConfig { extractEmail: Right (\_ -> pure unit) }
  in
    describe "CirclesPink.Garden.StateMachine.Stories" do
      describe "A user can signup" do
        it "ends up in `trusts` state" do
          ( signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
              # execScripT'
              # runTestEnvM
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
              # execScripT'
              # runTestEnvM
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
              # execScripT'
              # runTestEnvM
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
              # execScripT'
              # runTestEnvM
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
              # execScripT'
              # runTestEnvM
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
--                         lookup (convert sampleSafeAddress) st.trusts
--                           <#> (const unit)
--                   }
--           )
--     )
--       `shouldEqual` (Just unit)
