module Test.Wallet.PrivateKey where

import Prelude
import Data.String (length)
import Test.Unit as T
import Test.Unit.Assert as A
import Wallet.PrivateKey as P

tests :: T.TestSuite
tests =
  T.suite "PrivateKey" do
    T.test "genPrivateKey" do
      key <- P.genPrivateKey <#> P.toString
      A.equal 68 (length key)
