module VoucherServer.Main where

import Prelude

import CirclesPink.Data.Address (parseAddress, sampleAddress)
import CirclesPink.Data.Address as C
import Data.DateTime (diff)
import Data.DateTime.Instant (instant, toDateTime)
import Data.DateTime.Instant as DT
import Data.Either (Either(..), note)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Number (fromString)
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Params (class DecodeParam, decodeParam)
import Payload.Server.Response as Response
import Payload.Spec (POST, Spec(Spec))
import Simple.JSON (class WriteForeign, writeImpl)
import Web3.Accounts (SignatureObj(..))
import Web3.Accounts as W3A

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

instance decodeParamAddress :: DecodeParam Address where
  decodeParam x = decodeParam x
    >>= (parseAddress >>> note "Could not parse Address")
    <#> Address

--------------------------------------------------------------------------------

type ErrGetVoucher = String

type Voucher =
  { voucherCode :: String
  }

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

db :: Map Address (Array Voucher)
db =
  M.fromFoldable
    [ Address sampleAddress /\ []
    ]

allowedDiff ∷ Seconds
allowedDiff = Seconds 60.0

isValid :: SignatureObj -> Aff Boolean
isValid (SignatureObj { message, messageHash }) = do
  timestamp <- liftEffect $ toDateTime <$> now
  let
    messageValid = messageHash == (un W3A.Hash $ W3A.hashMessage message)
    maybeMessageTime = fromString message <#> Milliseconds >>= instant <#> toDateTime
    timestampValid = case maybeMessageTime of
      Nothing -> false
      Just i -> diff i timestamp <= allowedDiff
  pure (messageValid && timestampValid)

getVouchers :: { body :: { signatureObj :: SignatureObj } } -> Aff (Either Failure (Array Voucher))
getVouchers { body: { signatureObj } } = do
  case W3A.recover signatureObj of
    Left _ -> pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))
    Right address -> do
      valid <- isValid signatureObj
      if valid then
        M.lookup (Address $ C.Address address) db # fold # Right # pure
      else pure $ Left $ Error (Response.unauthorized (StringBody "UNAUTHORIZED"))

--------------------------------------------------------------------------------
main :: Effect Unit
main = Payload.launch spec
  { getVouchers
  }

