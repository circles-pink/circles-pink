module VoucherServer.Main where

import Prelude

import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Address as C
import Data.DateTime.Instant as DT
import Data.Either (Either, note)
import Data.Map (Map, empty)
import Data.Newtype (class Newtype, un)
import Data.Time (Millisecond)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref, modify_, new)
import Network.Ethereum.Core.HexString (unHex)
import Network.Ethereum.Core.Signatures (unAddress)
import Payload.ResponseTypes (Failure, Response)
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

type ServerState = Map Address Challenge

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

--------------------------------------------------------------------------------
spec
  :: Spec
       { getChallenge ::
           GET "/get-challenge/<address>"
             { params :: { address :: Address }
             , response :: Challenge
             }
       , getVouchers ::
           POST "/vouchers/<address>"
             { body :: ChallengeAnswer
             , params :: { address :: Address }
             , response :: Array Voucher
             }
       }
spec = Spec

--------------------------------------------------------------------------------
getChallenge :: Ref ServerState -> { params :: { address :: Address } } -> Aff Challenge
getChallenge ref { params: { address } } = do
  timestamp <- liftEffect now <#> Instant
  let
    challenge =
      { timestamp
      , message: "hello"
      }
  pure challenge

-- modify_ address challenge

getVouchers :: Ref ServerState -> { params :: { address :: Address }, body :: ChallengeAnswer } -> Aff (Either Failure (Array Voucher))
getVouchers ref = todo

--------------------------------------------------------------------------------
main :: Effect Unit
main = do
  ref <- new empty
  Payload.launch spec
    { getChallenge: getChallenge ref
    , getVouchers: getVouchers ref
    }
  pure unit