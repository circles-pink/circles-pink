module VoucherServer.Main
  ( ErrGetVoucher
  , GQLError(..)
  , Message
  , ServerConfig
  , ServerEnv
  , allowedDiff
  , app
  , db
  , getTransactions
  , getVouchers
  , isValid
  , main
  , queryGql
  , spec
  ) where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (Address, parseAddress)
import CirclesPink.Data.Nonce (addressToNonce)
import CirclesPink.Data.SafeAddress (sampleSafeAddress)
import CirclesPink.Data.SafeAddress as C
import Control.Monad.Except (mapExceptT, runExceptT)
import Convertable (convert)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.BN (BN, fromDecimalStr)
import Data.Bifunctor (lmap)
import Data.DateTime (diff)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (un, unwrap, wrap)
import Data.Newtype.Extra ((-#))
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Debug (spy, spyWith)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, logShow)
import Effect.Exception as E
import Effect.Now (now)
import GraphQL.Client.Args (type (==>), (=>>))
import GraphQL.Client.Query (query_)
import GraphQL.Client.Types (class GqlQuery)
import Node.Process (exit, getEnv)
import Payload.Client (mkClient)
import Payload.Client as PC
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server (Server, defaultOpts)
import Payload.Server as Payload
import Payload.Server.Response as Response
import Payload.Spec (POST, Spec(Spec))
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import TypedEnv (type (<:), envErrorMessage, fromEnv)
import VoucherServer.GraphQLSchemas.GraphNode (Schema)
import VoucherServer.GraphQLSchemas.GraphNode as GraphNode
import VoucherServer.Specs.Xbge (SafeAddress(..), xbgeSpec)
import VoucherServer.Types (EurCent(..), Voucher(..), VoucherAmount(..), VoucherCode(..), VoucherEncrypted(..), VoucherProviderId(..))
import Web3 (Message(..), SignatureObj(..), Web3, accountsHashMessage, accountsRecover, newWeb3_)

type Message =
  { id :: Int
  , text :: String
  }

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------
spec
  :: Spec
       { getVouchers ::
           POST "/vouchers"
             { body :: { signatureObj :: SignatureObj }
             , response :: Array Voucher
             }
       }
spec = Spec

--------------------------------------------------------------------------------

sampleVoucherOne :: Voucher
sampleVoucherOne = Voucher
  { voucherProviderId: VoucherProviderId "goodbuy"
  , voucherAmount: VoucherAmount $ EurCent 25
  , voucherCode: VoucherCode "bingo"
  , sold:
      { transactionId: "200-4"
      , safeAddress: show sampleSafeAddress
      , timestamp: "1659971225399"
      }
  }

sampleVoucherTwo :: Voucher
sampleVoucherTwo = Voucher
  { voucherProviderId: VoucherProviderId "goodbuy"
  , voucherAmount: VoucherAmount $ EurCent 15
  , voucherCode: VoucherCode "bongo"
  , sold:
      { transactionId: "201-4"
      , safeAddress: show sampleSafeAddress
      , timestamp: "1659971225500"
      }
  }

db :: Map SafeAddress (Array Voucher)
db = M.fromFoldable [ (wrap sampleSafeAddress) /\ [ sampleVoucherOne, sampleVoucherTwo ] ]

allowedDiff ∷ Seconds
allowedDiff = Seconds 60.0

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
                  log ("XBGE API Error: " <> show e)
                  pure $ Left $ Error (Response.internalError (StringBody "Internal error"))
                Right response -> case (response -# _.body) # traverse decryptVoucher of
                  Just voucherEncrypted -> pure $ Right voucherEncrypted
                  Nothing -> do
                    log "Decryption error"
                    pure $ Left $ Error (Response.internalError (StringBody "Internal error"))

        else pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))

decryptVoucher :: VoucherEncrypted -> Maybe Voucher
decryptVoucher = todo

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
  -> { fromAddress :: SafeAddress
     , toAddress :: SafeAddress
     }
  -> Aff (Maybe (Array Transfer))
getTransactions env { fromAddress, toAddress } = do
  result <- queryGql "get-transactions"
    { transfers:
        { where:
            { from: show fromAddress
            , to: show toAddress
            }
        } =>>
          { from: GraphNode.from
          , to: GraphNode.to
          , id: GraphNode.id
          , amount: GraphNode.amount
          }
    }
  case result of
    Left _ -> pure Nothing
    Right { transfers } -> transfers # traverse mkTransfer # pure

  where
  mkTransfer :: GraphNode.Transfer -> Maybe Transfer
  mkTransfer x = ado
    from <- parseAddress x.from
    to <- parseAddress x.to
    amount <- fromDecimalStr x.amount
    in { from, to, amount, id: x.id }

type Transfer =
  { from :: Address
  , to :: Address
  , id :: String
  , amount :: BN
  }

xbgeClient = mkClient
  ( PC.defaultOpts
      { baseUrl = "https://2j0bcp5tr9.execute-api.eu-central-1.amazonaws.com/dev"
      }
  )
  xbgeSpec

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
  => String
  -> query
  -> Aff (Either GQLError returns)
queryGql s q = query_ "http://graph.circles.local/subgraphs/name/CirclesUBI/circles-subgraph" (Proxy :: Proxy Schema) s q
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
  }

--------------------------------------------------------------------------------

app :: Aff (Either String Server)
app = do
  env <- liftEffect $ getEnv
  let config = lmap envErrorMessage $ fromEnv (Proxy :: _ ServerConfig) env
  case config of
    Left e -> do
      error e
      liftEffect $ exit 1
    Right parsedEnv -> case parsedEnv.port of
      Nothing -> Payload.start (defaultOpts { port = 4000 }) spec { getVouchers: getVouchers parsedEnv }
      Just port -> Payload.start (defaultOpts { port = port }) spec { getVouchers: getVouchers parsedEnv }

main :: Effect Unit
main = launchAff_ app
