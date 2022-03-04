module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Wallet.PrivateKey as Test.Wallet.PrivateKey
import CirclesPink.Garden.CirclesCore.Tests as CirclesPink.Garden.CirclesCore.Tests

foreign import globalRequires :: {}

main :: Effect Unit
main =
  runTest do
    Test.Wallet.PrivateKey.tests
    CirclesPink.Garden.CirclesCore.Tests.tests
