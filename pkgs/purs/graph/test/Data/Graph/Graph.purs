module Test.Data.Graph.Core where

import Prelude

import Debug.Extra (todo)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Data.Graph.Core" do
  it "bb" do
    1 `shouldEqual` 1