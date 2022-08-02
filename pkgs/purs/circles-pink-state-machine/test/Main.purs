module Test.CirclesPinkStateMachine.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import PursDeps.Tests as PursDeps.Tests
import Test.Data.BigInt as Test.Data.BigInt
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Unit.Main (runTest)
import Test.Wallet.PrivateKey as Test.Wallet.PrivateKey
import CirclesPink.Garden.StateMachine.Control.States.Dashboard as CirclesPink.Garden.StateMachine.Control.States.Dashboard
import Test.VoucherServer.Main as Test.VoucherServer.Main

foreign import globalRequires :: {}

mainTestUnit :: Effect Unit
mainTestUnit =
  runTest do
    Test.Wallet.PrivateKey.tests
    Test.Data.BigInt.tests
    PursDeps.Tests.tests

mainTestSpec :: Effect Unit
mainTestSpec = launchAff_ do
  specs <- discover """^(?!(Milkis.Impl.Window|cache-db\.json|Web3\.Bindings|Web3|Web.XHR.XMLHttpRequestUpload|Web.XHR|Web.Storage|Web.Internal.FFI|Web|VoucherServer.Main)).*"""
  runSpec [consoleReporter] specs

main :: Effect Unit
main = do
  mainTestSpec
  mainTestUnit
  Test.VoucherServer.Main.main
