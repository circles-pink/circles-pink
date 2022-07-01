module Storage.EncryptedStorage.Tests (spec) where

import Prelude
import Data.Argonaut (encodeJson)
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GunDB (get, offline, once, put)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec =
  describe "EncryptedStorage" do
    describe "Put" do
      it "Puts data" do
        -- gundb <- liftEffect $ offline
        -- ctx <- liftEffect $ gundb # get "users" # put (encodeJson { name: "Jane", surname: "Doe" })
        -- result <- (gundb # get "users" # once)
        -- let
        --   r = spy "res" result
        pure unit