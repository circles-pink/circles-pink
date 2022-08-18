module VoucherServer.Main
  ( encrypt
  , main
  ) where

import Prelude

import CirclesCore (SafeAddress(..))
import CirclesCore as CC
import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Nonce (addressToNonce)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT, throwError, withExceptT)
import Convertable (convert)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Array (find)
import Data.Array as A
import Data.BN (BN, fromDecimalStr)
import Data.Bifunctor (lmap)
import Data.DateTime (diff)
import Data.DateTime.Instant (Instant, instant, toDateTime, unInstant)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap, wrap)
import Data.Newtype.Extra ((-#))
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(..), convertDuration)
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
import VoucherServer.GraphQLSchemas.GraphNode (Schema, amount, from, id, time, to, transactionHash)
import VoucherServer.GraphQLSchemas.GraphNode as GraphNode
import VoucherServer.Guards.Auth (basicAuthGuard)
import VoucherServer.MonadApp (AppEnv(..), AppProdM, errorToLog, runAppProdM)
import VoucherServer.MonadApp.Class (errorToFailure)
import VoucherServer.MonadApp.Impl.Prod.AppEnv as Prod
import VoucherServer.Routes.TrustsReport (trustsReport) as Routes
import VoucherServer.Spec (spec)
import VoucherServer.Spec.Types (EurCent(..), Freckles(..), TransferId(..), Voucher(..), VoucherAmount(..), VoucherCode(..), VoucherCodeEncrypted(..), VoucherEncrypted(..), VoucherOffer(..), VoucherProvider(..), VoucherProviderId(..))
import VoucherServer.Specs.Xbge (Address(..), xbgeSpec)
import VoucherServer.Types (Transfer(..), TransferMeta(..))
import Web3 (Message(..), SignatureObj(..), Web3, accountsHashMessage, accountsRecover, newWeb3_)

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------

allowedDiff :: Seconds
allowedDiff = Seconds 60.0

supportedProvider :: VoucherProviderId
supportedProvider = VoucherProviderId "goodbuy"

mkSubgraphUrl :: String -> String -> String
mkSubgraphUrl url subgraphName = url <> "/subgraphs/name/" <> subgraphName

threshold :: Threshold EurCent
threshold = Threshold { above: EurCent 5, below: EurCent 5 }

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

  syncedVouchers <- for unfinalizedTxs (finalizeTx appEnv >>> runExceptT)
    # lift

  log ("finalized the following vouchers: ")
  logShow syncedVouchers

  pure unit

--------------------------------------------------------------------------------

almostEquals :: Threshold EurCent -> EurCent -> EurCent -> Boolean
almostEquals
  (Threshold { above: (EurCent above), below: (EurCent below) })
  (EurCent amount)
  (EurCent price) =
  let
    isInLowerRange = amount >= (price * 100 - below)
    isInUpperRange = amount <= (price * 100 + above)
  in
    isInLowerRange && isInUpperRange

newtype Threshold a = Threshold { above :: a, below :: a }

--------------------------------------------------------------------------------

getVoucherAmount :: Array VoucherProvider -> VoucherProviderId -> EurCent -> Maybe VoucherAmount
getVoucherAmount providers providerId payedAmount = do
  (VoucherProvider provider) <- find (\(VoucherProvider p) -> p.id == providerId) providers
  (VoucherOffer { amount }) <- find (\(VoucherOffer { amount: (VoucherAmount amount) }) -> almostEquals threshold payedAmount amount) provider.availableOffers
  pure amount

--------------------------------------------------------------------------------

redeemAmount :: AppEnvVars -> Address -> EurCent -> ExceptT String Aff Unit
redeemAmount _ _ _ =
  log "In the future we'll pay back the amount..."

--------------------------------------------------------------------------------

finalizeTx :: AppEnv AppProdM -> Transfer -> ExceptT String Aff VoucherEncrypted
finalizeTx (AppEnv { envVars }) (Transfer { from, amount, id }) = do
  let
    xbgeClient = mkClient (getOptions envVars) xbgeSpec

  (TransferMeta { time }) <- getTransferMeta envVars id # ExceptT

  providers <- xbgeClient.getVoucherProviders {}
    # ExceptT
    # withExceptT show
    <#> (unwrap >>> _.body >>> _.data)

  let
    eur = frecklesToEurCent time amount
    maybeVoucherAmount = getVoucherAmount providers supportedProvider eur

  case maybeVoucherAmount of
    Nothing -> do
      redeemAmount envVars from eur
      throwError "Invalid Voucher amount."
    Just voucherAmount ->
      xbgeClient.finalizeVoucherPurchase
        { body:
            { safeAddress: from
            , providerId: supportedProvider
            , amount: voucherAmount
            , transactionId: id
            }
        }
        # ExceptT
        # withExceptT show
        <#> (unwrap >>> _.body >>> _.data)

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

frecklesToEurCent :: Instant -> Freckles -> EurCent
frecklesToEurCent timestamp (Freckles freckles) =
  let
    ms = unInstant timestamp
  in
    frecklesToEuroCentImpl (unwrap ms) freckles # EurCent

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

getTransferMeta :: AppEnvVars -> TransferId -> Aff (Either String TransferMeta)
getTransferMeta (AppEnvVars env) transferId = do
  result <- queryGql (AppEnvVars env) "get-transfer-meta"
    { notifications:
        { where:
            { transfer: un TransferId transferId
            , safeAddress: show env.xbgeSafeAddress
            }
        } =>>
          { id, transactionHash, time }
    }
  pure $ case result of
    Left e -> Left $ show e
    Right { notifications } -> case A.head notifications of
      Just notification -> notification # mkTransferMeta
      Nothing -> Left "Return array is empty"

  where
  mkTransferMeta :: GraphNode.Notification -> Either String TransferMeta
  mkTransferMeta x = note "Parse error" ado
    time <- fromString x.time <#> Seconds <#> convertDuration >>= instant
    in TransferMeta { time, transactionHash: x.transactionHash, id: x.id }

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

foreign import frecklesToEuroCentImpl :: Number -> BN -> Int

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
