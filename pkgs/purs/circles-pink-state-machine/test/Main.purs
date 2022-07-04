module Test.CirclesPinkStateMachine.Main where

import Prelude
import CirclesCore.Tests as CirclesCore.Tests
import Test.CirclesPink.Garden.StateMachine.Control.States.Dashboard as Test.CirclesPink.Garden.StateMachine.Control.States.Dashboard
import Test.CirclesPink.Garden.StateMachine.Stories as Test.CirclesPink.Garden.StateMachine.Stories
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
        Test.CirclesPink.Garden.StateMachine.Control.States.Dashboard.spec
        --Test.CirclesPink.Garden.StateMachine.Stories.spec

main :: Effect Unit
main = do
  mainTestSpec
  mainTestUnit
