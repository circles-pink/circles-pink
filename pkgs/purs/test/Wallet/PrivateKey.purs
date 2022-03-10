module Test.Wallet.PrivateKey where

import Prelude
import Data.Array as Arr
import Data.String as S
import Test.Unit as T
import Test.Unit.Assert as A
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
      A.equal (P.nonceToInt $ P.addressToNonce P.sampleAddress) 22032785429977
