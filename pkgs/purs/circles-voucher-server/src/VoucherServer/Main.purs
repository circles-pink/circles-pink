module VoucherServer.Main where

import Prelude

import CirclesPink.Data.Address as C
import Data.DateTime.Instant as DT
import Data.Either (Either)
import Data.Newtype (class Newtype, un)
import Data.Time (Millisecond)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Network.Ethereum.Core.HexString (unHex)
import Network.Ethereum.Core.Signatures (unAddress)
import Payload.Server as Payload
import Payload.Server.Params (class DecodeParam, decodeParam)
import Payload.Spec (Spec(Spec), GET)
import Simple.JSON (class WriteForeign, writeImpl)

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

instance decodeParamAddress :: DecodeParam Address where
  decodeParam x =  todo :: Either String Address -- ?a
  
  -- mkHe
  
  -- un Address >>> ?a -- un C.Address  >>> ?a -- >>> unAddress >>> unHex >>> 
--------------------------------------------------------------------------------

type Challenge =
  { message :: String
  , timestamp :: Instant
  }

spec
  :: Spec
       { getMessages ::
           GET "/users/<id>/messages?limit=<limit>"
             { params :: { id :: Int }
             , query :: { limit :: Int }
             , response :: Array Message
             }
       , getVouchersPreflight ::
           GET "/vouchers-preflight/<address>"
             { params :: { address :: Address }
             , response :: Challenge
             }
       }
spec = Spec

getVouchersPreflight :: { params :: {address::Address} } -> Aff Challenge
getVouchersPreflight = todo

getMessages :: { params :: { id :: Int }, query :: { limit :: Int } } -> Aff (Array Message)
getMessages { params: { id }, query: { limit } } = pure
  [ { id: 1, text: "Hey " <> show id }, { id: 2, text: "Limit " <> show limit } ]

main :: Effect Unit
main = Payload.launch spec { getMessages, getVouchersPreflight }