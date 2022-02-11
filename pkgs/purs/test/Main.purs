module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec.Stadium.Reflect as Test.Spec.Stadium.Reflect
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    Test.Spec.Stadium.Reflect.tests
