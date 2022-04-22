module CirclesCore.Tests
  ( evalEffect_
  , spec
  ) where

import Prelude
import CirclesCore as CC
import Control.Monad.Except (lift, mapExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Wallet.PrivateKey as P

spec :: Spec Unit
spec =
  describe "CirclesCore" do
    describe "newWebSocketProvider" do
      it "Invalid URL" do
        (evalEffect_ $ CC.newWebSocketProvider "")
          >>= shouldEqual (Left $ CC._errInvalidUrl "")
      it "Valid URL" do
        (evalEffect_ $ CC.newWebSocketProvider "ws://localhost:8080")
          >>= shouldEqual (Right unit)
    describe "newWeb3" do
      it "Works" do
        ( evalEffect_ do
            provider <- CC.newWebSocketProvider "ws://localhost:8080"
            lift $ CC.newWeb3 provider
        )
          >>= shouldEqual (Right unit)
    describe "newCirclesCore" do
      it "Works" do
        ( evalEffect_ do
            provider <- CC.newWebSocketProvider "ws://localhost:8080"
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
    describe "safeDeploy" do
      it "Works" do
        ( evalAff_ do
            provider <- effToAff $ CC.newWebSocketProvider "ws://localhost:8080"
            web3 <- effToAff $ lift $ CC.newWeb3 provider
            core <-
              effToAff
                $ CC.newCirclesCore web3
                    { apiServiceEndpoint: "http://api.circles.xyz"
                    , graphNodeEndpoint: "http://graph.circles.xyz"
                    , hubAddress: "0x0000000000000000000000000000000000000000"
                    , proxyFactoryAddress: "0x0000000000000000000000000000000000000000"
                    , relayServiceEndpoint: "http://relay.circles.xyz"
                    , safeMasterAddress: "0x0000000000000000000000000000000000000000"
                    , subgraphName: ""
                    , databaseSource: ""
                    }
            account <- effToAff $ CC.privKeyToAccount web3 P.sampleKey
            CC.safeDeploy core account { safeAddress: P.sampleSafeAddress }
        )
          <#> lmap (const unit)
          >>= shouldEqual (Left unit)
    describe "safeIsFunded" do
      it "Works" do
        ( evalAff_ do
            provider <- effToAff $ CC.newWebSocketProvider "ws://localhost:8080"
            web3 <- effToAff $ lift $ CC.newWeb3 provider
            core <-
              effToAff
                $ CC.newCirclesCore web3
                    { apiServiceEndpoint: "http://api.circles.xyz"
                    , graphNodeEndpoint: "http://graph.circles.xyz"
                    , hubAddress: "0x0000000000000000000000000000000000000000"
                    , proxyFactoryAddress: "0x0000000000000000000000000000000000000000"
                    , relayServiceEndpoint: "http://relay.circles.xyz"
                    , safeMasterAddress: "0x0000000000000000000000000000000000000000"
                    , subgraphName: ""
                    , databaseSource: ""
                    }
            account <- effToAff $ CC.privKeyToAccount web3 P.sampleKey
            CC.safeIsFunded core account { safeAddress: P.sampleSafeAddress }
        )
          <#> lmap (const unit)
          >>= shouldEqual (Left unit)
    describe "tokenDeploy" do
      it "Works" do
        ( evalAff_ do
            provider <- effToAff $ CC.newWebSocketProvider "ws://localhost:8080"
            web3 <- effToAff $ lift $ CC.newWeb3 provider
            core <-
              effToAff
                $ CC.newCirclesCore web3
                    { apiServiceEndpoint: "http://api.circles.xyz"
                    , graphNodeEndpoint: "http://graph.circles.xyz"
                    , hubAddress: "0x0000000000000000000000000000000000000000"
                    , proxyFactoryAddress: "0x0000000000000000000000000000000000000000"
                    , relayServiceEndpoint: "http://relay.circles.xyz"
                    , safeMasterAddress: "0x0000000000000000000000000000000000000000"
                    , subgraphName: ""
                    , databaseSource: ""
                    }
            account <- effToAff $ CC.privKeyToAccount web3 P.sampleKey
            CC.tokenDeploy core account { safeAddress: P.sampleSafeAddress }
        )
          <#> lmap (const unit)
          >>= shouldEqual (Left unit)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
evalEffect :: forall a. ExceptV (CC.Err ()) Effect a -> Aff (Either (Variant (CC.Err ())) a)
evalEffect ev = liftEffect $ runExceptT $ ev

evalEffect_ :: forall a. ExceptV (CC.Err ()) Effect a -> Aff (Either (Variant (CC.Err ())) Unit)
evalEffect_ ev = evalEffect $ const unit <$> ev

evalAff :: forall a. ExceptV (CC.Err ()) Aff a -> Aff (Either (Variant (CC.Err ())) a)
evalAff ev = runExceptT $ ev

evalAff_ :: forall a. ExceptV (CC.Err ()) Aff a -> Aff (Either (Variant (CC.Err ())) Unit)
evalAff_ ev = evalAff $ const unit <$> ev

effToAff :: forall e a. ExceptV e Effect a -> ExceptV e Aff a
effToAff = mapExceptT liftEffect
