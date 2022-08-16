module Test.AllTests.Main where

import Prelude

import Effect (Effect)
import Test.VoucherServer.Main as Test.VoucherServer.Main

main :: Effect Unit
main = do
  Test.VoucherServer.Main.main
  --runSpec [consoleReporter] specs