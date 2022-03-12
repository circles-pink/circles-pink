module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Wallet.PrivateKey as Test.Wallet.PrivateKey
import CirclesPink.Garden.CirclesCore.Tests as CirclesPink.Garden.CirclesCore.Tests
import Test.Data.BigInt as Test.Data.BigInt
import PursDeps.Tests as PursDeps.Tests

foreign import globalRequires :: {}

main :: Effect Unit
main =
  runTest do
    Test.Wallet.PrivateKey.tests
    CirclesPink.Garden.CirclesCore.Tests.tests
    CirclesPink.Garden.CirclesCore.Tests.tests
    Test.Data.BigInt.tests
    PursDeps.Tests.tests
