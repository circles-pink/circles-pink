module CirclesPink.Garden.CirclesCore.Tests where

import Prelude
import CirclesPink.Garden.CirclesCore as CC
import Effect.Class (liftEffect)
import Test.Unit as T
import Test.Unit.Assert as A

tests :: T.TestSuite
tests =
  T.suite "CirclesPink.Garden.CirclesCore" do
    T.test "newCirclesCore" do
      provider <- liftEffect $ CC.newWebSocketProvider ""
      A.equal 1 1
