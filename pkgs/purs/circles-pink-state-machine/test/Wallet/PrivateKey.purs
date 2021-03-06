module Test.Wallet.PrivateKey where

import Prelude
-- import Data.Argonaut (JsonDecodeError(..), decodeJson, encodeJson)
-- import Data.Array as Arr
-- import Data.Either (Either(..))
-- import CirclesPink.Data.PrivateKey (PrivateKey)
-- import Data.String as S
import Test.Unit as T
-- import Test.Unit.Assert as A

tests :: T.TestSuite
tests = pure unit
-- T.suite "PrivateKey" do
--   T.test "genPrivateKey" do
--     key <- P.genPrivateKey <#> show
--     A.equal 66 (S.length key)
--   T.test "keyToMnemonic" do
--     key <- P.genPrivateKey
--     let
--       mnemonic = P.keyToMnemonic key
--     A.equal 24 (Arr.length $ P.getWords mnemonic)
--   T.test "roundtrip" do
--     key <- P.genPrivateKey
--     let
--       mnemonic = P.keyToMnemonic key
--     let
--       recoveredKey = P.mnemonicToKey mnemonic
--     A.equal (show key) (show recoveredKey)
--   T.suite "zeroKey" do
--     T.test "has correct length" do
--       A.equal (P.sampleKey # show # S.length) 66
--     T.test "produces right mnemonic" do
--       A.equal (P.sampleKey # P.keyToMnemonic # P.getWords)
--         [ "gym", "onion", "turkey", "slice", "blue", "random", "goat", "live", "grit", "educate", "slam", "alone", "enroll", "print", "need", "certain", "stumble", "addict", "drive", "accident", "iron", "provide", "major", "next" ]
--   T.test "addressToNonce" do
--     A.equal (P.nonceToString $ P.addressToNonce P.sampleAddress) "22032785429977"
--   T.test "isPrivateKey" do
--     A.equal true (P.isPrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c")
--   T.test "decodeJsonPrivateKey Happy" do
--     let
--       json = encodeJson P.sampleKey

--       pk = decodeJson json
--     A.equal (Right P.sampleKey) pk
--   T.test "decodeJsonPrivateKey Error" do
--     let
--       json = encodeJson "68135baae5b1856359041566a8d32c0374b355.."

--       pk :: _ _ PrivateKey
--       pk = decodeJson json
--     A.equal (Left $ TypeMismatch "Not a valid Private Key") pk
