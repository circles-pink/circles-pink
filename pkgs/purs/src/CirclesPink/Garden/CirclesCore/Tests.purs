module CirclesPink.Garden.CirclesCore.Tests where

import Prelude
import CirclesPink.Garden.CirclesCore as CC
import Control.Monad.Except (lift, runExceptT)
import Data.Either (Either(..), isRight)
import Data.Variant (Variant)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Unit as T
import Test.Unit.Assert as A

tests :: T.TestSuite
tests =
  T.suite "CirclesPink.Garden.CirclesCore" do
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
              }
          pure circlesCore
      case result of
        Left e -> log $ CC.printErr e
        Right _ -> pure unit
      A.assert "isRight" $ isRight result
