module VoucherServer.Main (main) where

import Prelude

import CirclesCore (SafeAddress(..))
import CirclesCore as CC
import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Nonce (addressToNonce)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, throwError, withExceptT)
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
import Data.Newtype (class Newtype, un, unwrap, wrap)
import Data.Newtype.Extra ((-#))
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(..), convertDuration)
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Debug (spyWith)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Effect.Exception as E
import Effect.Now (now)
import Effect.Timer (setInterval)
import GraphQL.Client.Args ((=>>))
import GraphQL.Client.Query (query_)
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
import TypedEnv (Resolved, Variable, envErrorMessage, fromEnv)
import VoucherServer.GraphQLSchemas.GraphNode (Schema, amount, from, id, time, to, transactionHash)
import VoucherServer.GraphQLSchemas.GraphNode as GraphNode
import VoucherServer.Spec (spec)
import VoucherServer.Specs.Xbge (Address(..), xbgeSpec)
import VoucherServer.Types (EurCent(..), Frackles(..), TransferId(..), Voucher(..), VoucherAmount(..), VoucherCode(..), VoucherCodeEncrypted(..), VoucherEncrypted(..), VoucherOffer(..), VoucherProvider(..), VoucherProviderId(..))
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

syncVouchers' :: ServerEnv -> ExceptT String Aff Unit
syncVouchers' env = do
  let
    xbgeClient = mkClient (getOptions env) xbgeSpec

  txs <- getTransactions env { toAddress: env.xbgeSafeAddress }
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

  syncedVouchers <- for unfinalizedTxs $ finalizeTx env

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
    isInLowerRange = amount >= (price - below)
    isInUpperRange = amount <= (price + above)
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

redeemAmount :: ServerEnv -> Address -> EurCent -> ExceptT String Aff Unit
redeemAmount _ _ _ =
  log "In the future we'll pay back the amount..."

--------------------------------------------------------------------------------

finalizeTx :: ServerEnv -> Transfer -> ExceptT String Aff VoucherEncrypted
finalizeTx env (Transfer { from, amount, id }) = do
  let
    xbgeClient = mkClient (getOptions env) xbgeSpec

  (TransferMeta { time }) <- getTransferMeta env id # ExceptT

  providers <- xbgeClient.getVoucherProviders {}
    # ExceptT
    # withExceptT show
    <#> (unwrap >>> _.body >>> _.data)

  let
    eur = fracklesToEurCent time amount
    maybeVoucherAmount = getVoucherAmount providers supportedProvider eur

  case maybeVoucherAmount of
    Nothing -> do
      redeemAmount env from eur
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

syncVouchers :: ServerEnv -> Aff Unit
syncVouchers env = do
  log "syncing transactions..."
  result <- runExceptT $ syncVouchers' env
  case result of
    Left e -> logShow e
    Right _ -> log "synced transactions."

getOptions :: ServerEnv -> Options
getOptions env = PC.defaultOpts
  { baseUrl = env.xbgeEndpoint
  , extraHeaders = H.fromFoldable [ "Authorization" /\ ("Basic " <> env.xbgeAuthSecret) ]
  -- , logLevel = Log
  }

getVoucherProviders :: ServerEnv -> {} -> Aff (Either Failure (Array VoucherProvider))
getVoucherProviders env _ = do
  let
    xbgeClient = mkClient (getOptions env) xbgeSpec
  result <- xbgeClient.getVoucherProviders {}
  case result of
    Left e -> do
      logShow e
      pure $ Left $ Error (Response.internalError (StringBody "Internal error"))
    Right response -> pure $ Right (response -# _.body # _.data)

getVouchers :: ServerEnv -> { body :: { signatureObj :: SignatureObj } } -> Aff (Either Failure (Array Voucher))
getVouchers env { body: { signatureObj } } = do
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
    xbgeClient = mkClient (getOptions env) xbgeSpec

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

fracklesToEurCent :: Instant -> Frackles -> EurCent
fracklesToEurCent timestamp (Frackles frackles) =
  let
    (seconds :: Seconds) = unInstant timestamp # convertDuration
  in
    frecklesToEuroCentImpl (unwrap seconds) frackles # EurCent

decryptVoucher :: String -> VoucherEncrypted -> Maybe Voucher
decryptVoucher key (VoucherEncrypted x) = ado
  code <- decryptVoucherCode key x.code
  in Voucher x { code = code }

decryptVoucherCode :: String -> VoucherCodeEncrypted -> Maybe VoucherCode
decryptVoucherCode key (VoucherCodeEncrypted s) = decrypt key s <#> VoucherCode

--------------------------------------------------------------------------------

getTransactions
  :: ServerEnv
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
    in Transfer { from, to, amount: Frackles amount, id: TransferId x.id }

newtype Transfer = Transfer
  { from :: Address
  , to :: Address
  , id :: TransferId
  , amount :: Frackles
  }

--------------------------------------------------------------------------------

getTransferMeta :: ServerEnv -> TransferId -> Aff (Either String TransferMeta)
getTransferMeta env transferId = do
  result <- queryGql env "get-transfer-meta"
    { notifications:
        { where: { transfer: un TransferId transferId } } =>>
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

newtype TransferMeta = TransferMeta
  { id :: String
  , transactionHash :: String
  , time :: Instant
  }

derive instance newtypeTransferMeta :: Newtype TransferMeta _

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
  => ServerEnv
  -> String
  -> query
  -> Aff (Either GQLError returns)
queryGql env s q = query_ (mkSubgraphUrl env.gardenGraphApi env.gardenSubgraphName) (Proxy :: Proxy Schema) s q
  # try
  <#> (lmap (spyWith "error" E.message >>> (\_ -> ConnOrParseError)))

--------------------------------------------------------------------------------

type ServerConfigSpec :: forall k. (Symbol -> Type -> k) -> Row k
type ServerConfigSpec f =
  ( port :: f "PORT" (Maybe Int)
  , gardenApi :: f "GARDEN_API" String
  , gardenApiUsers :: f "GARDEN_API_USERS" String
  , gardenGraphApi :: f "GARDEN_GRAPH_API" String
  , gardenSubgraphName :: f "GARDEN_SUBGRAPH_NAME" String
  , gardenRelay :: f "GARDEN_RELAY" String
  , gardenHubAddress :: f "GARDEN_HUB_ADDRESS" String
  , gardenProxyFactoryAddress :: f "GARDEN_PROXY_FACTORY_ADRESS" String
  , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" String
  , gardenEthereumNodeWebSocket :: f "GARDEN_ETHEREUM_NODE_WS" String
  , voucherCodeSecret :: f "VOUCHER_CODE_SECRET" String
  , xbgeAuthSecret :: f "XBGE_AUTH_SECRET" String
  , xbgeEndpoint :: f "XBGE_ENDPOINT" String
  , xbgeSafeAddress :: f "XBGE_SAFE_ADDRESS" Address
  )

type ServerConfig = ServerConfigSpec Variable

type ServerEnv = { | ServerConfigSpec Resolved }

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
  let config = lmap envErrorMessage $ fromEnv (Proxy :: _ ServerConfig) env
  case config of
    Left e -> do
      error e
      liftEffect $ exit 1
    Right parsedEnv -> do
      _ <- liftEffect $ setInterval 5000 (launchAff_ $ syncVouchers parsedEnv)
      _ <- Payload.start (defaultOpts { port = fromMaybe 4000 parsedEnv.port }) spec
        { getVouchers: getVouchers parsedEnv
        , getVoucherProviders: getVoucherProviders parsedEnv
        }
      pure $ Right unit

main :: Effect Unit
main = launchAff_ app
