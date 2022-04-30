module Test.Wallet.PrivateKey where

import Prelude
import Data.Argonaut (Json, decodeJson)
import Data.Array as Arr
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.String as S
import Test.Unit as T
import Test.Unit.Assert as A
import Wallet.PrivateKey (PrivateKey)
import Wallet.PrivateKey as P

tests :: T.TestSuite
tests =
  T.suite "PrivateKey" do
    T.test "genPrivateKey" do
      key <- P.genPrivateKey <#> P.toString
      A.equal 66 (S.length key)
    T.test "keyToMnemonic" do
      key <- P.genPrivateKey
      let
        mnemonic = P.keyToMnemonic key
      A.equal 24 (Arr.length $ P.getWords mnemonic)
    T.test "roundtrip" do
      key <- P.genPrivateKey
      let
        mnemonic = P.keyToMnemonic key
      let
        recoveredKey = P.mnemonicToKey mnemonic
      A.equal (P.toString key) (P.toString recoveredKey)
    T.suite "zeroKey" do
      T.test "has correct length" do
        A.equal (P.zeroKey # P.toString # S.length) 66
      T.test "produces right mnemonic" do
        A.equal (P.zeroKey # P.keyToMnemonic # P.getWords)
          [ "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "art" ]
    T.test "privKeyToAddress" do
      A.equal (P.addrToString $ P.privKeyToAddress P.sampleKey) (P.addrToString P.sampleAddress)
    T.test "addressToNonce" do
      A.equal (P.nonceToString $ P.addressToNonce P.sampleAddress) "22032785429977"
    T.test "isPrivateKey" do
      A.equal true (P.isPrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c")

-- T.test "decodeJsonPrivateKey" do
--   json <- pure encodeJson { privKey: "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c" }
--   result <- pure decodePrivKey json
--   pk <- case result of
--     Left _ -> Left (const false)
--     Right k -> Right k
--   A.equal P.sampleKey pk
decodePrivKey :: Json -> Either Boolean PrivateKey
decodePrivKey j = decodeJson j # lmap (const false)
