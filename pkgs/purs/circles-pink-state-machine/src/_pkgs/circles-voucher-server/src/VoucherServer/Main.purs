module VoucherServer.Main
  ( encrypt
  , main
  ) where

import Prelude

import CirclesCore (SafeAddress(..))
import CirclesCore as CC
import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Nonce (addressToNonce)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT, withExceptT)
import Convertable (convert)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Array as A
import Data.BN (fromDecimalStr)
import Data.Bifunctor (lmap)
import Data.DateTime (diff)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap, wrap)
import Data.Newtype.Extra ((-#))
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Effect.Now (now)
import Effect.Timer (setInterval)
import GraphQL.Client.Args ((=>>))
import GraphQL.Client.BaseClients.Urql (createClient)
import GraphQL.Client.Query (query)
import GraphQL.Client.Types (class GqlQuery)
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
import VoucherServer.GraphQLSchemas.GraphNode (Schema, amount, from, id, to)
import VoucherServer.GraphQLSchemas.GraphNode as GraphNode
import VoucherServer.Guards.Auth (basicAuthGuard)
import VoucherServer.MonadApp (AppEnv(..), AppProdM, errorToLog, runAppProdM)
import VoucherServer.MonadApp.Class (errorToFailure)
import VoucherServer.MonadApp.Impl.Prod.AppEnv as Prod
import VoucherServer.Routes.TrustsReport (trustsReport) as Routes
import VoucherServer.Spec (spec)
import VoucherServer.Spec.Types (Freckles(..), TransferId(..), Voucher(..), VoucherCode(..), VoucherCodeEncrypted(..), VoucherEncrypted(..), VoucherProvider)
import VoucherServer.Specs.Xbge (Address(..), xbgeSpec)
import VoucherServer.Sync as Sync
import VoucherServer.Types (Transfer(..))
import Web3 (Message(..), SignatureObj(..), Web3, accountsHashMessage, accountsRecover, newWeb3_)

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------

allowedDiff :: Seconds
allowedDiff = Seconds 60.0


mkSubgraphUrl :: String -> String -> String
mkSubgraphUrl url subgraphName = url <> "/subgraphs/name/" <> subgraphName

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

syncVouchers' :: AppEnv AppProdM -> ExceptT String Aff Unit
syncVouchers' appEnv@(AppEnv { envVars: AppEnvVars envVars }) = do
  let
    xbgeClient = mkClient (getOptions (AppEnvVars envVars)) xbgeSpec

  txs <- getTransactions (AppEnvVars envVars) { toAddress: envVars.xbgeSafeAddress }
    # ExceptT

  vouchers <- xbgeClient.getVouchers { query: { safeAddress: Nothing } }
    # ExceptT
    # withExceptT show
    <#> (unwrap >>> _.body >>> _.data)

  let
    vouchersLookup = vouchers
      <#> (\all@(VoucherEncrypted { sold: { transactionId } }) -> transactionId /\ all)
      # M.fromFoldable

    unfinalizedTxs = txs # A.filter (\(Transfer { id }) -> not $ M.member id vouchersLookup)

  syncedVouchers <- for unfinalizedTxs (finalizeTx' appEnv >>> runExceptT)
    # lift

  log ("finalized the following vouchers: ")
  logShow syncedVouchers

  pure unit

newtype Threshold a = Threshold { above :: a, below :: a }

--------------------------------------------------------------------------------

finalizeTx' :: AppEnv AppProdM -> Transfer -> ExceptT String Aff VoucherEncrypted
finalizeTx' appEnv trans = Sync.finalizeTx trans
  # runAppProdM appEnv
  # ExceptT
  # withExceptT errorToLog

syncVouchers :: AppEnv AppProdM -> Aff Unit
syncVouchers appEnv = do
  log "syncing transactions..."
  result <- runExceptT $ syncVouchers' appEnv
  case result of
    Left e -> logShow ("Could not sync transaction: " <> e)
    Right _ -> log "synced transactions."

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
  let _ = decryptVoucherCode key x.code
  let code = VoucherCode "Dummy Voucher Code"
  in Voucher x { code = code }

decryptVoucherCode :: String -> VoucherCodeEncrypted -> Maybe VoucherCode
decryptVoucherCode key (VoucherCodeEncrypted s) = decrypt key s <#> VoucherCode

--------------------------------------------------------------------------------

getTransactions
  :: AppEnvVars
  -> { toAddress :: Address
     }
  -> Aff (Either String (Array Transfer))
getTransactions env { toAddress } = do
  result <- queryGql env "get-transactions"
    { transfers:
        { where: { to: show toAddress } } =>> { from, to, id, amount }
    }
  case result of
    Left e -> pure $ Left $ show e
    Right { transfers } -> transfers # traverse mkTransfer # pure

  where
  mkTransfer :: GraphNode.Transfer -> Either String Transfer
  mkTransfer x = note "Parse error" ado
    from <- Address <$> parseAddress x.from
    to <- Address <$> parseAddress x.to
    amount <- fromDecimalStr x.amount
    in Transfer { from, to, amount: Freckles amount, id: TransferId x.id }

--------------------------------------------------------------------------------

data GQLError = ConnOrParseError

derive instance genericGQLError :: Generic GQLError _
instance showGQLError :: Show GQLError where
  show = genericShow

queryGql
  :: forall query returns
   . GqlQuery Schema query returns
  => DecodeJsonField returns
  => DecodeJson returns
  => AppEnvVars
  -> String
  -> query
  -> Aff (Either GQLError returns)
queryGql (AppEnvVars env) s q =
  do
    (client :: _ Schema _ _) <- liftEffect $ createClient { headers: [], url: (mkSubgraphUrl env.gardenGraphApi env.gardenSubgraphName) }
    query client s q # try <#> (lmap (\_ -> ConnOrParseError))

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
      prodEnv_ <- Prod.mkAppEnv # Prod.runM parsedEnv
      case prodEnv_ of
        Left e -> do
          error $ errorToLog e
          liftEffect $ exit 1
        Right prodEnv -> do
          let
            handlers =
              { getVouchers: getVouchers parsedEnv
              , getVoucherProviders: getVoucherProviders parsedEnv
              --, trustUsers: Routes.trustUsers parsedEnv >>> runWithLog
              , trustsReport: Routes.trustsReport >>> runRoute prodEnv
              }
            guards =
              { basicAuth: basicAuthGuard >>> runRoute prodEnv
              }
          _ <- liftEffect $ setInterval 5000 (launchAff_ $ syncVouchers prodEnv)
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

-- runWithLog :: forall a. ExceptT (String /\ Failure) Aff a -> Aff (Either Failure a)
-- runWithLog m = do
--   result <- runExceptT m
--   case result of
--     Left (msg /\ err) -> do
--       log msg
--       pure $ Left err
--     Right x -> pure $ Right x

main :: Effect Unit
main = launchAff_ app
