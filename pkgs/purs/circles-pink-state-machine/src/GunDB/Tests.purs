module GunDB.Tests (spec) where

import Prelude
import Data.Argonaut (encodeJson)
import Effect.Class (liftEffect)
import GunDB (get, offline, once, put)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Gun" do
    describe "Put" do
      it "Puts data" do
        gundb <- liftEffect $ offline
        _ <- liftEffect $ gundb # get "users" # put (encodeJson { name: "Jane", surname: "Doe" })
        _ <- (gundb # get "users" # once)
        pure unit

-- assertGunResult :: forall a b. Aff (Maybe { data :: { name :: String | a } | b }) -> String -> Aff Unit
-- assertGunResult aff name = aff >>= \res -> bound res name
--   where
--   bound :: forall c d. Maybe { data :: { name :: String | c } | d } -> String -> Aff Unit
--   bound (Just gunVal) expectedName = gunVal.data.name `shouldEqual` expectedName
--   bound Nothing _ = fail "No result"
