module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Wallet.PrivateKey as Test.Wallet.PrivateKey
import CirclesCore.Tests as CirclesCore.Tests
import Test.Data.BigInt as Test.Data.BigInt
import PursDeps.Tests as PursDeps.Tests

foreign import globalRequires :: {}

main :: Effect Unit
main =
  runTest do
    Test.Wallet.PrivateKey.tests
    CirclesCore.Tests.tests
    CirclesCore.Tests.tests
    Test.Data.BigInt.tests
    PursDeps.Tests.tests
