module Test.EncryptedStorage (spec) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Variant (Variant)
import EncryptStorage (ErrGetItem, _errItemNotFound, getItem, newEs)
import EncryptStorage.Bindings (EsOptions(..), SecretKey(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = pure unit
-- describe "EncryptedStorage" do
--   describe "newES" do
--     it "creates ES" do
--       result <- runExceptT do
--         es <- newEs (SecretKey "sk") (EsOptions { prefix: "1" })
--         pure es
--       pure unit
--   describe "Get" do
--     it "gets data" do
--       result <- runExceptT do
--         es <- newEs (SecretKey "sk") (EsOptions { prefix: "1" })
--         item :: Int <- getItem es "a"
--         pure item
--       result `shouldEqual` (Left $ (_errItemNotFound "a" :: Variant (ErrGetItem ())))
-- describe "Set" do
--   it "sets data" do
--     result <- runExceptT do
--       es <- newEs (SecretKey "sk") (EsOptions { prefix: "1" })
--       set <- setItem
--       item :: Int <- getItem es "a"
--       pure item
--     result `shouldEqual` (Left $ (_errItemNotFound "a" :: Variant (ErrGetItem ())))