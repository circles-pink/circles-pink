module Test.CirclesPink.Garden.StateMachine.Control.States.Dashboard where

import Prelude

import CirclesCore (User, _errNative)
import CirclesPink.Data.Address (Address)
import CirclesPink.Data.PrivateKey (sampleKey)
import CirclesPink.Garden.StateMachine.Control.Env (GetUsers)
import CirclesPink.Garden.StateMachine.Control.States.Dashboard as D
import CirclesPink.Garden.TestEnv (TestEnvT, runTestEnvM, testEnv)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Array (catMaybes, find)
import Data.Either (Either(..), hush)
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.TestUtils (addrA, addrB, addrC, addrD, addrE, unsafeMkAddr, userA, userB, userE)

safeFunderAddr :: Address
safeFunderAddr = unsafeMkAddr "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1"

--------------------------------------------------------------------------------
spec :: Spec Unit
spec =
  describe "CirclesPink.Garden.StateMachine.Control.States.Dashboard" do
    describe "fetchUsersBinarySearch" do
      it "empty array" do
        D.fetchUsersBinarySearch testEnv sampleKey []
          # run
          # shouldEqual (Just [])
      it "one element not in db" do
        let
          db =
            M.fromFoldable
              [ addrA /\ userA ]

          testEnv' = testEnv { getUsers = mkGetUsers db }
        D.fetchUsersBinarySearch testEnv' sampleKey [ addrB ]
          # run
          # shouldEqual (Just [ Left addrB ])
      it "one element in db" do
        let
          db =
            M.fromFoldable
              [ addrA /\ userA ]

          testEnv' = testEnv { getUsers = mkGetUsers db }
        D.fetchUsersBinarySearch testEnv' sampleKey [ addrA ]
          # run
          # shouldEqual (Just [ Right userA ])
      it "many elements in db" do
        let
          db =
            M.fromFoldable
              [ addrA /\ userA, addrB /\ userB, addrE /\ userE ]

          testEnv' = testEnv { getUsers = mkGetUsers db }
        D.fetchUsersBinarySearch testEnv' sampleKey [ addrA, addrB, addrC, addrD, addrE ]
          # run
          # shouldEqual (Just [ Right userA, Right userB, Left addrC, Left addrD, Right userE ])

-- describe "mapTrusts" do
--   it "runs" do
--     D.mapTrust [] (fromFoldable [])  `shouldEqual` 2

mkGetUsers :: Map Address User -> GetUsers (TestEnvT Identity)
mkGetUsers db _ _ xs =
  let
    result = map (\k -> M.lookup k db) xs
  in
    result
      # find (\x -> x == Nothing)
      # case _ of
          Just _ -> throwError $ _errNative { message: "", name: "" }
          Nothing -> pure $ catMaybes result

--------------------------------------------------------------------------------

run :: forall t16 t17. ExceptT t16 (TestEnvT Identity) t17 -> Maybe t17
run = runExceptT >>> runTestEnvM >>> hush

-- trustedUser :: User
-- trustedUser = { avatarUrl: "", id: 0, safeAddress: P.sampleAddress, username: "a" }

-- checkMap :: Boolean
-- checkMap =
--   let
--     users :: Array User
--     users = [ ]

--     trusts :: Map W3.Address Trust
--     trusts = fromFoldable [  ]

--     trustNode :: TrustNode
--     trustNode = { isIncoming: true, isOutgoing: true, limitPercentageIn: 50, limitPercentageOut: 50, mutualConnections: [], safeAddress: P.sampleAddress }

--     expectedResult :: W3.Address /\ Trust
--     expectedResult = undefined

--     actualResult :: W3.Address /\ Trust
--     actualResult = mapTrust users trusts trustNode
--   in
--     actualResult == expectedResult