module VoucherServer.Main
  ( Address(..)
  , ErrGetVoucher
  , GQLError(..)
  , Instant(..)
  , Message
  , Schema
  , ServerConfig
  , ServerEnv
  , Transaction
  , Transfer
  , allowedDiff
  , amount
  , app
  , db
  , from
  , getTransactions
  , getVouchers
  , id
  , isValid
  , main
  , name
  , prop
  , queryGql
  , sampleVoucher
  , spec
  , to
  ) where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (parseAddress, sampleAddress)
import CirclesPink.Data.Address as C
import CirclesPink.Data.Nonce (addressToNonce)
import Control.Monad.Except (mapExceptT, runExceptT)
import Convertable (convert)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField)
import Data.BN (BN)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.DateTime (diff)
import Data.DateTime.Instant (instant, toDateTime)
import Data.DateTime.Instant as DT
import Data.Either (Either(..), note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, wrap)
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Debug (spy, spyWith)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, logShow)
import Effect.Exception as E
import Effect.Now (now)
import GraphQL.Client.Args (class ArgGql, type (==>), (=>>))
import GraphQL.Client.Query (query_)
import GraphQL.Client.Types (class GqlQuery)
import Node.Process (exit, getEnv)
import Payload.Client (mkClient)
import Payload.Client as PC
import Payload.Client.EncodeParam (class EncodeParam, encodeParam)
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server (Server, defaultOpts)
import Payload.Server as Payload
import Payload.Server.Params (class DecodeParam, decodeParam)
import Payload.Server.Response as Response
import Payload.Spec (POST, Spec(Spec))
import Simple.JSON (class WriteForeign, writeImpl)
import Type.Proxy (Proxy(..))
import TypedEnv (type (<:), envErrorMessage, fromEnv)
import VoucherServer.Specs.Xbge (xbgeSpec)
import VoucherServer.Types (Voucher)
import Web3 (Message(..), SignatureObj(..), Web3, accountsHashMessage, accountsRecover, newWeb3_)

type Message =
  { id :: Int
  , text :: String
  }

--------------------------------------------------------------------------------
newtype Instant = Instant DT.Instant

derive instance newtypeInstant :: Newtype Instant _

instance writeForeignInstant :: WriteForeign Instant where
  writeImpl = un Instant >>> DT.unInstant >>> un Milliseconds >>> writeImpl

--------------------------------------------------------------------------------

newtype Address = Address C.Address

derive instance newtypeAddress :: Newtype Address _

derive newtype instance ordAddress :: Ord Address
derive newtype instance eqAddress :: Eq Address
derive newtype instance decodeJsonAddress :: DecodeJson Address
derive newtype instance showAddress :: Show Address

instance decodeParamAddress :: DecodeParam Address where
  decodeParam x = decodeParam x
    >>= (parseAddress >>> note "Could not parse Address")
    <#> Address

instance argGqlAddress :: ArgGql Address String

--------------------------------------------------------------------------------

newtype SafeAddress = SafeAddress CC.SafeAddress

derive instance newtypeSafeAddress :: Newtype SafeAddress _

derive newtype instance ordSafeAddress :: Ord SafeAddress
derive newtype instance eqSafeAddress :: Eq SafeAddress
derive newtype instance decodeJsonSafeAddress :: DecodeJson SafeAddress
derive newtype instance showSafeAddress :: Show SafeAddress

instance decodeParamSafeAddress :: DecodeParam SafeAddress where
  decodeParam x = decodeParam x
    >>= (parseAddress >>> note "Could not parse SafeAddress")
    <#> SafeAddress

instance encodeParamSafeAddress :: EncodeParam SafeAddress where
  encodeParam (SafeAddress x) = encodeParam x

instance argGqlSafeAddress :: ArgGql SafeAddress String

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

sampleVoucher :: Voucher
sampleVoucher = { voucherCode: "Bingo" }

db :: Map Address (Array Voucher)
db = M.fromFoldable [ Address sampleAddress /\ [ sampleVoucher ] ]

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
    Left _ -> pure $ Left $ Error (Response.internalError (StringBody "INTERNAL SERVER ERROR"))
    Right cc -> case accountsRecover web3 signatureObj of
      Nothing -> pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))
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
                  { saltNonce: nonce
                  , owners: [ convert address ]
                  , threshold: 1
                  }
              }
          case safeAddress of
            Left _ -> pure $ Left $ Error (Response.notFound (StringBody "SAFE ADDRESS NOT FOUND"))
            Right sa -> do
              txs <- getTransactions env (wrap sa)
              M.lookup (Address sa) db # fold # Right # pure
        else pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))

--------------------------------------------------------------------------------

type Transaction = {}

getTransactions :: forall r. { | r } -> Address -> Aff (Maybe (Array Transaction))
getTransactions env addr = do
  result <- queryGql "Hallo? "
    { transfers:
        { where:
            { from: "0x5281764e3ce70ebc6c2eb18aaa8c4aebff98f393"
            , to: "0x2390b428f0a968b39eb4d7cbec8d20bcd578c411"
            }
        } =>> { from, to, id, amount }
    }
  case result of
    Left e -> logShow e
    Right { transfers } -> logShow transfers
  pure $ Just [ {} ]

xbgeClient = mkClient
  ( PC.defaultOpts
      { baseUrl = "..."
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

-- Schema
type Schema =
  { transfers :: { where :: { from :: Address, to :: Address } } ==> Array Transfer
  }

type Transfer =
  { from :: Address
  , to :: Address
  , id :: String
  , amount :: BN
  }

-- Symbols 
prop :: Proxy "prop"
prop = Proxy

name :: Proxy "name"
name = Proxy

from :: Proxy "from"
from = Proxy

to :: Proxy "to"
to = Proxy

id :: Proxy "id"
id = Proxy

amount :: Proxy "amount"
amount = Proxy

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
