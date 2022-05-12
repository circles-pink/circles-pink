module CirclesPink.Garden.StateMachine.Control.States.Dashboard.Tests where

import Prelude
import CirclesCore (User, _errNative)
import CirclesPink.Garden.Env (testEnv)
import CirclesPink.Garden.StateMachine.Control.Env (GetUsers)
import CirclesPink.Garden.StateMachine.Control.States.Dashboard as D
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Convertable (convert)
import Data.Argonaut (decodeJson, fromString)
import Data.Array (catMaybes, find)
import Data.Either (Either(..), hush)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Wallet.PrivateKey as P

safeFunderAddr :: W3.Address
safeFunderAddr = unsafeMkAddr "0x90F8bf6A479f320ead074411a4B0e7944Ea8c9C1"

--------------------------------------------------------------------------------
addrA :: W3.Address
addrA = unsafeMkAddr "0x0142e59D7e0744e984aCa46Bbe9A7eF5C3Fa50ba"

userA :: User
userA = mkDummyUser addrA

--
addrB :: W3.Address
addrB = unsafeMkAddr "0x01F19302779CfB177b1F928386FCd61eE6856057"

userB :: User
userB = mkDummyUser addrB

--
addrC :: W3.Address
addrC = unsafeMkAddr "0x02B50e87C577084b9659a625870b4A6e8a8E9238"

userC :: User
userC = mkDummyUser addrC

--
addrD :: W3.Address
addrD = unsafeMkAddr "0x041653a75c0238Fe4382Ed36A36BbD6F71C0f52B"

userD :: User
userD = mkDummyUser addrD

--
addrE :: W3.Address
addrE = unsafeMkAddr "0x07E23d9A3c09AD80aa9acF0bcEcF41cD3B06FdD0"

userE :: User
userE = mkDummyUser addrE

--
mkDummyUser :: W3.Address -> User
mkDummyUser a = { id: 1, username: "A", avatarUrl: "", safeAddress: convert $ a }

--------------------------------------------------------------------------------
spec :: Spec Unit
spec =
  describe "CirclesPink.Garden.StateMachine.Control.States.Dashboard" do
    describe "fetchUsersBinarySearch" do
      it "empty array" do
        D.fetchUsersBinarySearch testEnv P.sampleKey []
          # run
          # shouldEqual (Just [])
      it "one element not in db" do
        let
          db =
            M.fromFoldable
              [ addrA /\ userA ]

          testEnv' = testEnv { getUsers = mkGetUsers db }
        D.fetchUsersBinarySearch testEnv' P.sampleKey [ addrB ]
          # run
          # shouldEqual (Just [ Left addrB ])
      it "one element in db" do
        let
          db =
            M.fromFoldable
              [ addrA /\ userA ]

          testEnv' = testEnv { getUsers = mkGetUsers db }
        D.fetchUsersBinarySearch testEnv' P.sampleKey [ addrA ]
          # run
          # shouldEqual (Just [ Right userA ])
      it "many elements in db" do
        let
          db =
            M.fromFoldable
              [ addrA /\ userA, addrB /\ userB, addrE /\ userE ]

          testEnv' = testEnv { getUsers = mkGetUsers db }
        D.fetchUsersBinarySearch testEnv' P.sampleKey [ addrA, addrB, addrC, addrD, addrE ]
          # run
          # shouldEqual (Just [ Right userA, Right userB, Left addrC, Left addrD, Right userE ])

mkGetUsers :: Map W3.Address User -> GetUsers Identity
mkGetUsers db _ _ xs =
  let
    result = map (\k -> M.lookup (convert k) db) xs
  in
    result
      # find (\x -> x == Nothing)
      # case _ of
          Just _ -> throwError $ _errNative { message: "", name: "" }
          Nothing -> pure $ catMaybes result

--------------------------------------------------------------------------------
unsafeMkAddr :: String -> W3.Address
unsafeMkAddr str =
  unsafePartial
    ( str
        # fromString
        # decodeJson
        # hush
        # fromJust
    )

run :: forall t16 t17. ExceptT t16 Identity t17 -> Maybe t17
run = runExceptT >>> (\(Identity x) -> x) >>> hush
