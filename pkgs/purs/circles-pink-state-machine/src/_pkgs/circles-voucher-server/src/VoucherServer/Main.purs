module VoucherServer.Main
  ( encrypt
  , main
  ) where

import Prelude

import CirclesCore (SafeAddress(..))
import CirclesCore as CC
import CirclesPink.Data.Nonce (addressToNonce)
import Control.Monad.Except (mapExceptT, runExceptT)
import Convertable (convert)
import Data.Bifunctor (lmap)
import Data.DateTime (diff)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap, wrap)
import Data.Newtype.Extra ((-#))
import Data.Number (fromString)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Node.Process (exit, getEnv)
import Payload.Client (ClientError(..), Options, mkClient)
import Payload.Client as PC
import Payload.Headers as H
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server (defaultOpts)
import Payload.Server as Payload
import Payload.Server.Response as Response
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import TypedEnv (envErrorMessage, fromEnv)
import VoucherServer.EnvVars (AppEnvVars(..), AppEnvVarsSpec)
import VoucherServer.Guards.Auth (basicAuthGuard)
import VoucherServer.MonadApp (AppEnv, AppProdM, errorToLog, runAppProdM)
import VoucherServer.MonadApp.Class (AppConstants, errorToFailure)
import VoucherServer.MonadApp.Impl.Prod.AppEnv as Prod
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (runMkAppProdM)
import VoucherServer.Routes.TrustUsers (trustUsers) as Routes
import VoucherServer.Routes.TrustsReport (trustsReport) as Routes
import VoucherServer.Spec (spec)
import VoucherServer.Spec.Types (Voucher(..), VoucherCode(..), VoucherCodeEncrypted(..), VoucherEncrypted(..), VoucherProvider)
import VoucherServer.Specs.Xbge (xbgeSpec)
import VoucherServer.Sync as Sync
import Web3 (Message(..), SignatureObj(..), Web3, accountsHashMessage, accountsRecover, newWeb3_)

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------

allowedDiff :: Seconds
allowedDiff = Seconds 60.0

--------------------------------------------------------------------------------

isValid :: Web3 -> SignatureObj -> Aff Boolean
isValid web3 (SignatureObj { message, messageHash }) = do
  timestamp <- liftEffect $ toDateTime <$> now
  let
    messageValid = messageHash == accountsHashMessage web3 message
    maybeMessageTime = message # un Message # fromString <#> Milliseconds >>= instant <#> toDateTime
    timestampValid = case maybeMessageTime of
      Nothing -> false
      Just i -> diff i timestamp <= allowedDiff

  pure (messageValid && timestampValid)

newtype Threshold a = Threshold { above :: a, below :: a }

--------------------------------------------------------------------------------

getOptions :: AppEnvVars -> Options
getOptions (AppEnvVars env) = PC.defaultOpts
  { baseUrl = env.xbgeEndpoint
  , extraHeaders = H.fromFoldable [ "Authorization" /\ ("Basic " <> env.xbgeAuthSecret) ]
  -- , logLevel = Log
  }

getVoucherProviders :: AppEnvVars -> {} -> Aff (Either Failure (Array VoucherProvider))
getVoucherProviders env _ = do
  let
    xbgeClient = mkClient (getOptions env) xbgeSpec
  result <- xbgeClient.getVoucherProviders {}
  case result of
    Left e -> do
      logShow e
      pure $ Left $ Error (Response.internalError (StringBody "Internal error"))
    Right response -> pure $ Right (response -# _.body # _.data)

getVouchers :: AppEnvVars -> { body :: { signatureObj :: SignatureObj } } -> Aff (Either Failure (Array Voucher))
getVouchers (AppEnvVars env) { body: { signatureObj } } = do
  web3 <- newWeb3_
  circlesCore <- runExceptT $ mapExceptT liftEffect $ CC.newCirclesCore web3
    { apiServiceEndpoint: env.gardenApi
    , graphNodeEndpoint: env.gardenGraphApi
    , hubAddress: env.gardenHubAddress
    , proxyFactoryAddress: env.gardenProxyFactoryAddress
    , relayServiceEndpoint: env.gardenRelay
    , safeMasterAddress: env.gardenSafeMasterAddress
    , subgraphName: env.gardenSubgraphName
    , databaseSource: "graph"
    }

  let
    xbgeClient = mkClient (getOptions (AppEnvVars env)) xbgeSpec

  case circlesCore of
    Left _ -> do
      log "no circles core"
      pure $ Left $ Error (Response.internalError (StringBody "INTERNAL SERVER ERROR"))
    Right cc -> case accountsRecover web3 signatureObj of
      Nothing -> do
        log "Unauthorized error"
        pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))
      Just address -> do
        valid <- isValid web3 signatureObj

        if valid then do
          let nonce = addressToNonce address
          safeAddress <- runExceptT $
            CC.utilsRequestRelayer cc
              { path: [ "safes", "predict" ]
              , version: 3
              , method: "POST"
              , data:
                  { saltNonce: coerce nonce
                  , owners: [ convert address ]
                  , threshold: 1
                  }
              }
          case safeAddress of
            Left _ -> do
              log "Safe Address not found"
              pure $ Left $ Error (Response.notFound (StringBody "SAFE ADDRESS NOT FOUND"))
            Right (SafeAddress sa) -> do
              result <- xbgeClient.getVouchers
                { query: { safeAddress: Just $ wrap $ wrap sa }
                }
              case result of
                Left e -> do
                  case e of
                    DecodeError { error } -> do
                      log ("DecodeError: " <> show error)
                      pure unit
                    StatusError _ -> log "StatusError"
                    RequestError _ -> log "RequestError"
                  log ("XBGE API Error: ")

                  pure $ Left $ Error (Response.internalError (StringBody "Internal error"))
                Right response -> case (response -# _.body # _.data) # traverse (decryptVoucher env.voucherCodeSecret) of
                  Just voucherEncrypted -> pure $ Right voucherEncrypted
                  Nothing -> do
                    log "Decryption error"
                    pure $ Left $ Error (Response.internalError (StringBody "Internal error"))

        else pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))

decryptVoucher :: String -> VoucherEncrypted -> Maybe Voucher
decryptVoucher key (VoucherEncrypted x) = ado
  code <- decryptVoucherCode key x.code
  in Voucher x { code = code }

decryptVoucherCode :: String -> VoucherCodeEncrypted -> Maybe VoucherCode
decryptVoucherCode key (VoucherCodeEncrypted s) = decrypt key s <#> VoucherCode

--------------------------------------------------------------------------------

foreign import decryptImpl :: forall z. z -> (String -> z) -> String -> String -> z

decrypt :: String -> String -> Maybe String
decrypt = decryptImpl Nothing Just

foreign import encryptImpl :: forall z. z -> (String -> z) -> String -> String -> z

encrypt :: String -> String -> Maybe String
encrypt = encryptImpl Nothing Just

--------------------------------------------------------------------------------

app :: Aff (Either String Unit)
app = do
  env <- liftEffect $ getEnv
  let
    config = fromEnv (Proxy :: _ AppEnvVarsSpec) env
      # lmap envErrorMessage
      <#> AppEnvVars
  case config of
    Left e -> do
      error e
      liftEffect $ exit 1
    Right parsedEnv -> do
      prodEnv_ <- Prod.mkAppEnv # runMkAppProdM { envVars: parsedEnv, constants: appConstants }
      case prodEnv_ of
        Left e -> do
          error $ errorToLog e
          liftEffect $ exit 1
        Right prodEnv -> do
          let
            handlers =
              { getVouchers: getVouchers parsedEnv
              , getVoucherProviders: getVoucherProviders parsedEnv
              , trustUsers: Routes.trustUsers >>> runRoute prodEnv
              , trustsReport: Routes.trustsReport >>> runRoute prodEnv
              }
            guards =
              { basicAuth: basicAuthGuard >>> runRoute prodEnv
              }
          _ <- liftEffect $ setInterval 5000 (launchAff_ $ runSync prodEnv Sync.syncVouchers)
          _ <- Payload.startGuarded (defaultOpts { port = fromMaybe 4000 (unwrap parsedEnv).port })
            spec
            { guards, handlers }
          pure $ Right unit

runRoute :: forall a. AppEnv AppProdM -> AppProdM a -> Aff (Either Failure a)
runRoute env x = do
  result <- runAppProdM env x
  case result of
    Left appError -> do
      log $ errorToLog appError
      pure $ Left $ errorToFailure appError
    Right ok -> pure $ Right ok

runSync :: forall a. AppEnv AppProdM -> AppProdM a -> Aff Unit
runSync env x = do
  result <- runAppProdM env x
  case result of
    Left appError -> do
      log $ errorToLog appError
    Right _ -> pure unit

-- runWithLog :: forall a. ExceptT (String /\ Failure) Aff a -> Aff (Either Failure a)
-- runWithLog m = do
--   result <- runExceptT m
--   case result of
--     Left (msg /\ err) -> do
--       log msg
--       pure $ Left err
--     Right x -> pure $ Right x

appConstants :: AppConstants
appConstants =
  { trustLimitPercentage: 100.0
  }

main :: Effect Unit
main = launchAff_ app
