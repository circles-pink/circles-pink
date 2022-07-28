module Test.PursTs.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ do
  specs <- discover """(Test\.Data\.PursTs\..*|Test\.PursTs\..*)"""
  runSpec [consoleReporter] specs