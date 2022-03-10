module CirclesPink.Garden.Env
  ( env, testEnv
  ) where

import Prelude
import CirclesPink.Garden.CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control as C
import CirclesPink.Garden.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Identity (Identity)
import Data.Variant (inj)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import HTTP (ReqFn)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Wallet.PrivateKey (sampleAddress, zeroKey)
import Wallet.PrivateKey as P

_errService :: CirclesError
_errService = inj (Proxy :: _ "errService") unit

_errParse :: CirclesError
_errParse = inj (Proxy :: _ "errParse") unit

env :: { request :: ReqFn (CirclesError' ()) } -> C.Env Aff
env { request } =
  { apiCheckUserName:
      \username ->
        if username == "" then
          pure { isValid: false }
        else
          request
            { url: "https://api.circles.garden/api/users/"
            , method: POST
            , body: encodeJson { username }
            }
            # runExceptT
            -- <#> (spy "log")
            
            <#> ( \result -> case result of
                  Left e -> Left e
                  Right x
                    | x.status /= 200 && x.status /= 409 -> Left $ _errService
                    | otherwise -> Right x
              )
            <#> ( \result -> do
                  res <- result
                  body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
                  if body'.status == "ok" then
                    Right { isValid: true }
                  else
                    Right { isValid: false }
              )
            # ExceptT
  , apiCheckEmail:
      \email ->
        if email == "" then
          pure { isValid: false }
        else
          request
            { url: "https://api.circles.garden/api/users/"
            , method: POST
            , body: encodeJson { email }
            }
            # runExceptT
            -- <#> (spy "log")
            
            <#> ( \result -> case result of
                  Left e -> Left e
                  Right x
                    | x.status /= 200 && x.status /= 409 -> Left $ _errService
                    | otherwise -> Right x
              )
            <#> ( \result -> do
                  res <- result
                  body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
                  if body'.status == "ok" then
                    Right { isValid: true }
                  else
                    Right { isValid: false }
              )
            # ExceptT
  , generatePrivateKey: P.genPrivateKey
  , userRegister:
      \options ->
        mapExceptT liftEffect do
          web3 <- getWeb3
          circlesCore <- getCirclesCore web3
          CC.userRegister circlesCore options
  , getSafeAddress:
      \{ nonce, privKey } -> do
        web3 <- mapExceptT liftEffect getWeb3
        circlesCore <- mapExceptT liftEffect $ getCirclesCore web3
        account <- mapExceptT liftEffect $ CC.privKeyToAccount web3 privKey
        address <- CC.safePredictAddress circlesCore account { nonce: nonce }
        pure address
  }

getWeb3 = do
  provider <- CC.newWebSocketProvider "ws://localhost:8545"
  web3 <- lift $ CC.newWeb3 provider
  pure web3

getCirclesCore web3 =
  CC.newCirclesCore web3
    { apiServiceEndpoint: "https://api.circles.garden"
    , graphNodeEndpoint: "https://api.thegraph.com"
    , hubAddress: "0xCfEB869F69431e42cdB54A4F4f105C19C080A601"
    , proxyFactoryAddress: "0xD833215cBcc3f914bD1C9ece3EE7BF8B14f841bb"
    , relayServiceEndpoint: "https://relay.circles.garden"
    , safeMasterAddress: "0xC89Ce4735882C9F0f0FE26686c53074E09B0D550"
    , subgraphName: "CirclesUBI/circles-subgraph"
    }

testEnv :: C.Env Identity
testEnv =
  { apiCheckUserName: \_ -> pure { isValid: true }
  , apiCheckEmail: \_ -> pure { isValid: true }
  , generatePrivateKey: pure zeroKey
  , userRegister: \_ -> pure unit
  , getSafeAddress: \_ -> pure sampleAddress
  }
