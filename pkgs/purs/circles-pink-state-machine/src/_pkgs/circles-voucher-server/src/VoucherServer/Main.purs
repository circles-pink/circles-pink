module VoucherServer.Main (main) where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Nonce (addressToNonce)
import Control.Comonad.Env (ask)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, withExceptT)
import Convertable (convert)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.Array as A
import Data.BN (BN, fromDecimalStr)
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
import Debug (spyWith)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Effect.Exception as E
import Effect.Now (now)
import Effect.Timer (setInterval)
import Effect.Unsafe (unsafePerformEffect)
import GraphQL.Client.Args ((=>>))
import GraphQL.Client.Query (query_)
import GraphQL.Client.Types (class GqlQuery)
import GunDB (offline)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Process (exit, getEnv)
import Payload.Client (ClientError(..), Options, mkClient)
import Payload.Client as PC
import Payload.Client.ClientApi (class ClientApi)
import Payload.Client.Options (LogLevel(..))
import Payload.Headers as H
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server (Server, defaultOpts)
import Payload.Server as Payload
import Payload.Server.Response as Response
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import TypedEnv (type (<:), envErrorMessage, fromEnv)
import VoucherServer.GraphQLSchemas.GraphNode (Schema)
import VoucherServer.GraphQLSchemas.GraphNode as GraphNode
import VoucherServer.Spec (spec)
import VoucherServer.Specs.Xbge (Address(..), xbgeSpec)
import VoucherServer.Types (EurCent(..), Frackles(..), Voucher(..), VoucherAmount(..), VoucherCode(..), VoucherCodeEncrypted(..), VoucherEncrypted(..), VoucherProvider(..), VoucherProviderId(..))
import Web3 (Message(..), SignatureObj(..), Web3, accountsHashMessage, accountsRecover, newWeb3_)

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------

allowedDiff :: Seconds
allowedDiff = Seconds 60.0

supportedProvider :: VoucherProviderId
supportedProvider = VoucherProviderId "goodbuy"

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
    <#> (\response -> response -# _.body # _.data)

  let
    vouchersLookup = vouchers
      <#> (\all@(VoucherEncrypted { sold: { transactionId } }) -> transactionId /\ all)
      # M.fromFoldable

    unfinalizedTxs = txs # A.filter (\(Transfer { id }) -> not $ M.member id vouchersLookup)

  syncedVouchers <- for unfinalizedTxs $ finalizeTx env

  log ("finalized the following vouchers: ")
  logShow syncedVouchers

  pure unit

finalizeTx :: ServerEnv -> Transfer -> ExceptT String Aff VoucherEncrypted
finalizeTx env (Transfer { from, amount, id }) = do
  let
    xbgeClient = mkClient (getOptions env) xbgeSpec

  xbgeClient.finalizeVoucherPurchase
    { body:
        { safeAddress: from
        , providerId: supportedProvider
        , amount: VoucherAmount $ fracklesToEurCent amount
        , transactionId: id
        }
    }
    # ExceptT
    # withExceptT show
    <#> (\response -> response -# _.body # _.data)

syncVouchers :: ServerEnv -> Aff Unit
syncVouchers env = do
  result <- runExceptT $ syncVouchers' env
  case result of
    Left e -> logShow e
    Right _ -> log "syncing transactions..."

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
    Left e -> pure $ Left $ Error (Response.internalError (StringBody "Internal error"))
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
            Right sa -> do
              result <- xbgeClient.getVouchers
                { query: { safeAddress: Just $ wrap $ wrap sa }
                }
              case result of
                Left e -> do
                  case e of
                    DecodeError { error, response } -> do
                      log ("DecodeError: " <> show error)
                      let x = unsafePerformEffect $ writeTextFile UTF8 "out.json" (_.body $ unwrap response)
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

fracklesToEurCent :: Frackles -> EurCent
fracklesToEurCent = todo

decryptVoucher :: String -> VoucherEncrypted -> Maybe Voucher
decryptVoucher key (VoucherEncrypted x) = ado
  code <- pure $ coerce x.code --   decryptVoucherCode key x.code
  in Voucher x { code = code }

decryptVoucherCode :: String -> VoucherCodeEncrypted -> Maybe VoucherCode
decryptVoucherCode key (VoucherCodeEncrypted s) = decrypt key s <#> VoucherCode

-- do
--   txs <- getTransactions env
--     { fromAddress: SafeAddress $ C.SafeAddress sa
--     , toAddress: SafeAddress $ C.SafeAddress sa
--     }
--   let _ = spy "Transactions" txs
--   M.lookup (SafeAddress $ C.SafeAddress sa) db # fold # Right # pure

--------------------------------------------------------------------------------

getTransactions
  :: ServerEnv
  -> { toAddress :: Address
     }
  -> Aff (Either String (Array Transfer))
getTransactions env { toAddress } = do
  result <- queryGql env "get-transactions"
    { transfers:
        { where:
            { to: show toAddress
            }
        } =>>
          { from: GraphNode.from
          , to: GraphNode.to
          , id: GraphNode.id
          , amount: GraphNode.amount
          }
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
    in Transfer { from, to, amount: Frackles amount, id: x.id }

newtype Transfer = Transfer
  { from :: Address
  , to :: Address
  , id :: String
  , amount :: Frackles
  }

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
queryGql env s q = query_ (env.gardenGraphApi <> "/subgraphs/name/" <> env.gardenSubgraphName) (Proxy :: Proxy Schema) s q
  # try
  <#> (lmap (spyWith "error" E.message >>> (\_ -> ConnOrParseError)))

-- {
--   transfers (
--     where: 
--     {
--       from: "idFrom",
--       to: "idTo"
--     }
--   )
--   {
--     id 
--     from 
--     to 
--     amount
--   }
-- }

-- {
--   notifications (
--     where: 
--     {
--       transfer: "21429855-18"
--       safeAddress: "0xccdfa2fa15c9d0ba7e84a96341a54296873abba4"
--     }
--   ) 
--   {
--     id
--     safeAddress
--     transactionHash
--     transfer {from to amount}
--   }
-- }

--------------------------------------------------------------------------------

type ServerConfig =
  ( port :: Maybe Int <: "PORT"
  , gardenApi :: String <: "GARDEN_API"
  , gardenApiUsers :: String <: "GARDEN_API_USERS"
  , gardenGraphApi :: String <: "GARDEN_GRAPH_API"
  , gardenSubgraphName :: String <: "GARDEN_SUBGRAPH_NAME"
  , gardenRelay :: String <: "GARDEN_RELAY"
  , gardenHubAddress :: String <: "GARDEN_HUB_ADDRESS"
  , gardenProxyFactoryAddress :: String <: "GARDEN_PROXY_FACTORY_ADRESS"
  , gardenSafeMasterAddress :: String <: "GARDEN_SAFE_MASTER_ADDRESS"
  , gardenEthereumNodeWebSocket :: String <: "GARDEN_ETHEREUM_NODE_WS"
  , voucherCodeSecret :: String <: "VOUCHER_CODE_SECRET"
  , xbgeAuthSecret :: String <: "XBGE_AUTH_SECRET"
  , xbgeEndpoint :: String <: "XBGE_ENDPOINT"
  , xbgeSafeAddress :: Address <: "XBGE_SAFE_ADDRESS"
  )

type ServerEnv =
  { port :: Maybe Int
  , gardenApi :: String
  , gardenApiUsers :: String
  , gardenGraphApi :: String
  , gardenSubgraphName :: String
  , gardenRelay :: String
  , gardenHubAddress :: String
  , gardenProxyFactoryAddress :: String
  , gardenSafeMasterAddress :: String
  , gardenEthereumNodeWebSocket :: String
  , voucherCodeSecret :: String
  , xbgeAuthSecret :: String
  , xbgeEndpoint :: String
  , xbgeSafeAddress :: Address
  }

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
  let config = lmap envErrorMessage $ fromEnv (Proxy :: _ ServerConfig) env
  case config of
    Left e -> do
      error e
      liftEffect $ exit 1
    Right parsedEnv -> do
      _ <- liftEffect $ setInterval 5000 (launchAff_ $ syncVouchers parsedEnv)
      -- Payload.start (defaultOpts { port = fromMaybe 4000 parsedEnv.port }) spec
      --   { getVouchers: getVouchers parsedEnv
      --   , getVoucherProviders: getVoucherProviders parsedEnv
      --   }
      pure $ Right unit

main :: Effect Unit
main = launchAff_ app
