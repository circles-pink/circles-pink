module Test.EncryptedStorage (spec) where

import Prelude

import Test.Spec (Spec)

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