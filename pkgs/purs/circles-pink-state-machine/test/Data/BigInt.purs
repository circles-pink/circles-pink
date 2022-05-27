module Test.Data.BigInt where

import Prelude
import Data.BigInt as B
import Test.Unit as T
import Test.Unit.Assert as A

tests :: T.TestSuite
tests =
  T.suite "Data.BigInt" do
    T.test "roundtrip" do
      let
        n = "22032785429977"
      A.equal (B.fromString n # B.toString) n
