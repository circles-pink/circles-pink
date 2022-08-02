module Test.AllTests.Main where

import Prelude

import Effect (Effect)
import Test.VoucherServer.Main as Test.VoucherServer.Main

main :: Effect Unit
main = launchAff_ do
  specs <- discover """^(?!(Milkis.Impl.Window|cache-db\.json|Web3\.Bindings|Web3|Web.XHR.XMLHttpRequestUpload|Web.XHR|Web.Storage|Web.Internal.FFI|Web|VoucherServer.Main)).*"""
  runSpec [consoleReporter] specs
