module VoucherServer.Main where

import Prelude

import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Address as C
import Data.DateTime.Instant as DT
import Data.Either (Either, note)
import Data.Newtype (class Newtype, un)
import Data.Time (Millisecond)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Network.Ethereum.Core.HexString (unHex)
import Network.Ethereum.Core.Signatures (unAddress)
import Payload.Server as Payload
import Payload.Server.Params (class DecodeParam, DecodeError, decodeParam)
import Payload.Spec (GET, Spec(Spec), POST)
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
  decodeParam x = decodeParam x
    >>= (parseAddress >>> note "Could not parse Address")
    <#> Address

--------------------------------------------------------------------------------

type Challenge =
  { message :: String
  , timestamp :: Instant
  }

type ChallengeAnswer =
  { signature :: String
  }

type ErrGetVoucher = String

type Voucher =
  { voucherCode :: String
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
       , getVouchers ::
           POST "/vouchers/<address>"
             { body :: ChallengeAnswer
             , guards :: { address :: Address }
             , response :: Either ErrGetVoucher (Array Voucher)
             }
       }
spec = Spec

getVouchersPreflight :: { params :: { address :: Address } } -> Aff Challenge
getVouchersPreflight = todo

getVouchers :: { guards :: { address :: Address } } -> Aff (Either ErrGetVoucher (Array Voucher))
getVouchers = todo

getMessages :: { params :: { id :: Int }, query :: { limit :: Int } } -> Aff (Array Message)
getMessages { params: { id }, query: { limit } } = pure
  [ { id: 1, text: "Hey " <> show id }, { id: 2, text: "Limit " <> show limit } ]

main :: Effect Unit
main = Payload.launch spec { getMessages, getVouchersPreflight, getVouchers }