module Test.Main where

import Prelude
import CirclesCore.Tests as CirclesCore.Tests
import CirclesPink.Garden.StateMachine.Control.States.Dashboard.Tests as CirclesPink.Garden.StateMachine.Control.States.Dashboard.Tests
import CirclesPink.Garden.StateMachine.Stories.Tests as CirclesPink.Garden.StateMachine.Stories.Tests
import Effect (Effect)
import Effect.Aff (launchAff_)
import GunDB.Tests as GunDB.Tests
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
    Test.Data.BigInt.tests
    PursDeps.Tests.tests

mainTestSpec :: Effect Unit
mainTestSpec =
  launchAff_
    $ runSpec [ consoleReporter ] do
        CirclesCore.Tests.spec
        GunDB.Tests.spec
        CirclesPink.Garden.StateMachine.Control.States.Dashboard.Tests.spec
        CirclesPink.Garden.StateMachine.Stories.Tests.spec

main :: Effect Unit
main = do
  mainTestSpec
  mainTestUnit
