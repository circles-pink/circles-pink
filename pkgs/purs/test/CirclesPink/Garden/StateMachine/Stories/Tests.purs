module CirclesPink.Garden.StateMachine.Stories.Tests
  ( spec
  ) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "CirclesPink.Garden.StateMachine.Stories" do
    describe "bar" do
      it "foo" do
        1
          # shouldEqual 2
