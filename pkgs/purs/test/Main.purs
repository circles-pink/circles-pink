module Test.Main where

import Prelude
import CirclesCore.Tests as CirclesCore.Tests
import Effect (Effect)
import Effect.Aff (launchAff_)
import PursDeps.Tests as PursDeps.Tests
import Test.Data.BigInt as Test.Data.BigInt
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Unit.Main (runTest)
import Test.Wallet.PrivateKey as Test.Wallet.PrivateKey

foreign import globalRequires :: {}

mainTestUnit :: Effect Unit
mainTestUnit =
  runTest do
    Test.Wallet.PrivateKey.tests
    CirclesCore.Tests.tests
    CirclesCore.Tests.tests
    Test.Data.BigInt.tests
    PursDeps.Tests.tests

mainTestSpec :: Effect Unit
mainTestSpec =
  launchAff_
    $ runSpec [ consoleReporter ] do
        CirclesCore.Tests.spec

main :: Effect Unit
main = do
  mainTestSpec
  mainTestUnit
