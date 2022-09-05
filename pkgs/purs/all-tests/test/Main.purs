module Test.AllTests.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  specs <- discover """^(?!(Milkis.Impl.Window|cache-db\.json|Web3\.Bindings|Web3|Web.XHR.XMLHttpRequestUpload|Web.XHR|Web.Storage|Web.Internal.FFI|Web|VoucherServer.Main)).*"""
  runSpec [consoleReporter] specs
