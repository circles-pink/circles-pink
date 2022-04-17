module CirclesCore.Tests where

import Prelude
import CirclesCore as CC
import Control.Monad.Except (lift, runExceptT)
import Data.Either (Either(..), isRight)
import Data.Variant (Variant)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Unit as T
import Test.Unit.Assert as A

tests :: T.TestSuite
tests =
  T.suite "CirclesCore" do
    T.test "newWebSocketProvider" do
      result <-
        liftEffect $ runExceptT $ CC.newWebSocketProvider ""
      case result of
        Left e -> log $ CC.printErr e
        Right _ -> pure unit
      A.assert "isRight" $ isRight result
    T.test "newCirclesCore" do
      result :: Either (Variant (CC.Err ())) CC.CirclesCore <-
        (liftEffect <<< runExceptT) do
          provider <- CC.newWebSocketProvider "ws://localhost:8545"
          web3 <- lift $ CC.newWeb3 provider
          circlesCore <-
            CC.newCirclesCore web3
              { apiServiceEndpoint: ""
              , graphNodeEndpoint: ""
              , hubAddress: "0x0000000000000000000000000000000000000000"
              , proxyFactoryAddress: "0x0000000000000000000000000000000000000000"
              , relayServiceEndpoint: ""
              , safeMasterAddress: "0x0000000000000000000000000000000000000000"
              , subgraphName: ""
              , databaseSource: ""
              }
          pure circlesCore
      case result of
        Left e -> log $ CC.printErr e
        Right _ -> pure unit
      A.assert "isRight" $ isRight result

spec :: Spec Unit
spec =
  describe "CirclesCore" do
    describe "newWebSocketProvider" do
      it "awesome" do
        1 `shouldEqual` 1
