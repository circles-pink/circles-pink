module CirclesCore.Gun (spec) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gun (get, offline, once, put)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "Gun" do
    describe "Put" do
      it "Puts data" do
        gundb <- liftEffect offline
        ctx <- liftEffect $ gundb # get "users" # put { name: "John", surname: "Doe" }
        assertGunResult (gundb # get "users" # once) "John"

assertGunResult :: forall a b. Aff (Maybe { data :: { name :: String | a } | b }) -> String -> Aff Unit
assertGunResult aff name = aff >>= \res -> bound res name
  where
  bound :: forall c d. Maybe { data :: { name :: String | c } | d } -> String -> Aff Unit
  bound (Just gunVal) expectedName = gunVal.data.name `shouldEqual` expectedName

  bound Nothing _ = fail "No result"
