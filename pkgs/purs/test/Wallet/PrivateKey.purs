module Test.Wallet.PrivateKey where

import Prelude
import Data.Array as Arr
import Data.String (length)
import Test.Unit as T
import Test.Unit.Assert as A
import Wallet.PrivateKey as P

tests :: T.TestSuite
tests =
  T.suite "PrivateKey" do
    T.test "genPrivateKey" do
      key <- P.genPrivateKey <#> P.toString
      A.equal 66 (length key)
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
