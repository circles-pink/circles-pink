module CirclesCore.Tests
  ( evalEffect_
  , spec
  , tests
  ) where

import Prelude
import CirclesCore as CC
import Control.Monad.Except (lift, runExceptT, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Unit as T
import Test.Unit.Assert as A
import Type.Proxy (Proxy(..))

tests :: T.TestSuite
tests =
  T.suite "CirclesCore" do
    T.test "newWebSocketProvider" do
      result <-
        liftEffect $ runExceptT $ CC.newWebSocketProvider "ws://localhost:8545"
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
      it "Invalid URL" do
        (evalEffect_ $ CC.newWebSocketProvider "")
          >>= shouldEqual (Left $ CC._errInvalidUrl "")
      it "Valid URL" do
        (evalEffect_ $ CC.newWebSocketProvider "ws://localhost:8545")
          >>= shouldEqual (Right unit)
    describe "newWeb3" do
      it "Works" do
        ( evalEffect_ do
            provider <- CC.newWebSocketProvider "ws://localhost:8545"
            lift $ CC.newWeb3 provider
        )
          >>= shouldEqual (Right unit)
    describe "newCirclesCore" do
      it "Works" do
        ( evalEffect_ do
            provider <- CC.newWebSocketProvider "ws://localhost:8545"
            web3 <- lift $ CC.newWeb3 provider
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
        )
          >>= shouldEqual (Right unit)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
evalEffect :: forall a. ExceptV (CC.Err ()) Effect a -> Aff (Either (Variant (CC.Err ())) a)
evalEffect ev = liftEffect $ runExceptT $ ev

evalEffect_ :: forall a. ExceptV (CC.Err ()) Effect a -> Aff (Either (Variant (CC.Err ())) Unit)
evalEffect_ ev = evalEffect $ const unit <$> ev
