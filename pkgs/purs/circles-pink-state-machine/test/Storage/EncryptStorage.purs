module Test.Storage.EncryptStorage where

import Prelude

import Data.Argonaut (JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array as Arr
import Data.Either (Either(..))
import CirclesPink.Data.PrivateKey (PrivateKey)
import Data.String as S
import Test.Unit as T
import Test.Unit.Assert as A

tests :: T.TestSuite
tests = pure unit
-- T.suite "PrivateKey" do
--   T.test "genPrivateKey" do
--     key <- P.genPrivateKey <#> show
--     A.equal 66 (S.length key)