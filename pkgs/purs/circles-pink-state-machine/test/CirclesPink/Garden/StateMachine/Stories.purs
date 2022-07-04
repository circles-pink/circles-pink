module Test.CirclesPink.Garden.StateMachine.Stories
  -- ( spec
  -- ) 
  where

import Prelude

import CirclesCore (ErrInvalidUrl, ErrNative, ErrService, ErrParseAddress)
import CirclesPink.Data.Mnemonic (sampleMnemonic)
import CirclesPink.Garden.StateMachine.Config (CirclesConfig(..))
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Common (ErrReadyForDeployment)
import CirclesPink.Garden.StateMachine.Control.Env (Env)
import CirclesPink.Garden.StateMachine.State (CirclesState, UserData, DashboardState)
import CirclesPink.Garden.StateMachine.Stories (finalizeAccount, loginUser, signUpUser)
import CirclesPink.Garden.TestEnv (TestEnvM, liftEnv, runTestEnvM, testEnv)
import Control.Monad.State (class MonadState, StateT, State)
import Data.Either (Either(..))
import Data.FpTs.Tuple (type (/\))
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Data.Variant.Extra (getLabel)
import Debug.Extra (todo)
import Effect.Aff (Aff)
import Log.Class (class MonadLog)
import RemoteReport (RemoteReport)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import CirclesPink.Garden.StateMachine.State.Dashboard (RedeployTokenResult)
import Type.Row (type (+))
import CirclesPink.Garden.StateMachine.Control.Env (ErrDeployToken)

--------------------------------------------------------------------------------

-- newtype CirclesState' = CirclesState' CirclesState

-- derive instance newtypeCirclesState' :: Newtype CirclesState' _

-- --------------------------------------------------------------------------------

-- type R r = (a :: Int | r)

-- type RR r = R + r

-- type ErrDeployToken2 r = ErrService + ErrInvalidUrl + ErrNative + ErrParseAddress + r

-- type RedeployTokenResult2 = RemoteReport
--   (Variant (ErrDeployToken2  + () ))
--   String

-- newtype TestScriptM a = TestScriptM (StateT (RedeployTokenResult2 ) Aff a)

-- instance monadCirclesTestScriptM :: MonadCircles TestScriptM where
--   sleep = todo

-- instance monadLogTestScriptM :: MonadLog TestScriptM where
--   log = todo

-- derive newtype instance monadStateTestScriptM :: MonadState CirclesState TestScriptM

-- derive newtype instance monadTestScriptM :: Monad TestScriptM

-- derive newtype instance functorTestScriptM :: Functor TestScriptM

-- runTestScriptM :: forall a. TestScriptM a -> a /\ CirclesState
-- runTestScriptM = todo

-- --------------------------------------------------------------------------------
-- spec :: Spec Unit
-- spec =
--   let
--     env' :: Env (StateT CirclesState TestEnvM)
--     env' = liftEnv testEnv

--     cfg = CirclesConfig { extractEmail: Right (\_ -> pure unit) }
--   in
--     describe "CirclesPink.Garden.StateMachine.Stories" do
--       describe "A user can signup" do
--         it "ends up in `trusts` state" do
--           ( signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
--               # runTestScriptM
--               # getLabel
--           )
--             `shouldEqual`
--               "trusts"
--       describe "A user can finalize the account" do
--         it "ends up in `dashboard` state" do
--           ( ( do
--                 _ <- signUpUser env' cfg { username: "Foo", email: "foo@bar.com" }
--                 _ <- finalizeAccount env' cfg
--                 pure unit
--             )
--               # runTestScriptM
--               # getLabel
--           )
--             `shouldEqual`
--               "dashboard"
--       describe "A user can not login with invalid mnemonic" do
--         it "stays in `login` state" do
--           ( ( do
--                 _ <- loginUser env' cfg { magicWords: "" }
--                 pure unit
--             )
--               # runTestScriptM
--               # getLabel
--           )
--             `shouldEqual`
--               "login"
--       describe "A user can not login with unregistered account" do
--         it "stays in `login` state" do
--           ( ( do
--                 _ <- loginUser env' cfg { magicWords: "volcano agree attack fiction firm chunk sweet private average undo pen core plunge choose vendor way liar depth romance enjoy hire rhythm little later" }
--                 pure unit
--             )
--               # runTestScriptM
--               # getLabel
--           )
--             `shouldEqual`
--               "login"
--       describe "A user can login" do
--         it "ends up in `dashboard` state" do
--           ( ( do
--                 _ <- loginUser env' cfg { magicWords: show sampleMnemonic }
--                 pure unit
--             )
--               # runTestScriptM
--               # getLabel
--           )
--             `shouldEqual`
--               "dashboard"

-- -- -- describe "A user can trust" do
-- -- --   it "can trust another user" do
-- -- --     ( ( do
-- -- --           _ <- loginUser env' { magicWords: show sampleMnemonic }
-- -- --           _ <- trustUser env' { safeAddress: addrToString sampleSafeAddress }
-- -- --           pure unit
-- -- --       )
-- -- --         # (\x -> spy "b" x)
-- -- --         # execScripT'
-- -- --         # runTestEnvM
-- -- --         # (\x -> spy "a" x)
-- -- --         #
-- -- --           ( default Nothing
-- -- --               # onMatch
-- -- --                   { dashboard: \st ->
-- -- --                       let
-- -- --                         x = spy "x" (addrToString sampleSafeAddress)
-- -- --                         y = spy "y" $ (toUnfoldable st.trusts :: Array _)
-- -- --                       in
-- -- --                         lookup (convert sampleSafeAddress) st.trusts
-- -- --                           <#> (const unit)
-- -- --                   }
-- -- --           )
-- -- --     )
-- -- --       `shouldEqual` (Just unit)
