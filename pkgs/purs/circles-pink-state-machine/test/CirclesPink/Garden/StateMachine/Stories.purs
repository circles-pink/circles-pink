module Test.CirclesPink.Garden.StateMachine.Stories
  ( spec
  ) where

import Prelude

import CirclesPink.Garden.Env (TestEnvM, liftEnv, runTestEnvM, testEnv)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState)
import CirclesPink.Garden.StateMachine.Stories (ScriptT, execScripT', finalizeAccount, loginUser, runScripT, signUpUser, trustUser)
import Control.Monad.State (StateT)
import Convertable (convert)
import Data.Map (lookup, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Tuple (snd)
import Data.Variant (default, onMatch)
import Data.Variant.Extra (getLabel)
import Debug (spy)
import Debug.Extra (todo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Wallet.PrivateKey (addrToString, sampleMnemonic, sampleSafeAddress)

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
      describe "A user can not login with invalid mnemonic" do
        it "stays in `login` state" do
          ( ( do
                _ <- loginUser env' { magicWords: "" }
                pure unit
            )
              # execScripT'
              # runTestEnvM
              # getLabel
          )
            `shouldEqual` "login"
      describe "A user can not login with unregistered account" do
        it "stays in `login` state" do
          ( ( do
                _ <- loginUser env' { magicWords: "volcano agree attack fiction firm chunk sweet private average undo pen core plunge choose vendor way liar depth romance enjoy hire rhythm little later" }
                pure unit
            )
              # execScripT'
              # runTestEnvM
              # getLabel
          )
            `shouldEqual` "login"
      describe "A user can login" do
        it "ends up in `dashboard` state" do
          ( ( do
                _ <- loginUser env' { magicWords: show sampleMnemonic }
                pure unit
            )
              # execScripT'
              # runTestEnvM
              # getLabel
          )
            `shouldEqual` "dashboard"
      describe "A user can trust" do
        it "can trust another user" do
          ( ( do
                _ <- loginUser env' { magicWords: show sampleMnemonic }
                _ <- trustUser env' { safeAddress: addrToString sampleSafeAddress }
                pure unit
            )
              # (\x -> spy "b" x)
              # execScripT'
              # runTestEnvM
              # (\x -> spy "a" x)
              #
                ( default Nothing
                    # onMatch
                        { dashboard: \st ->
                            let
                              x = spy "x" (addrToString sampleSafeAddress)
                              y = spy "y" $ (toUnfoldable st.trusts :: Array _)
                            in
                              lookup (convert sampleSafeAddress) st.trusts
                                <#> (const unit)
                        }
                )
          )
            `shouldEqual` (Just unit)
