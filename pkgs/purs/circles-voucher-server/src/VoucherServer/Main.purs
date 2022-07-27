module VoucherServer.Main where

import Prelude

import CirclesPink.Data.Address (parseAddress, sampleAddress)
import CirclesPink.Data.Address as C
import Control.Comonad.Env (ask)
import Data.DateTime.Instant as DT
import Data.Either (Either(..), note)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as M
import Data.Newtype (class Newtype, un)
import Data.Tuple.Nested ((/\))
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Payload.ResponseTypes (Failure)
import Payload.Server as Payload
import Payload.Server.Params (class DecodeParam, decodeParam)
import Payload.Spec (POST, Spec(Spec))
import Simple.JSON (class WriteForeign, writeImpl)
import Web3.Accounts (SignatureObj)
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
db = M.fromFoldable
  [ Address sampleAddress /\ []
  ]

isValid :: SignatureObj -> Aff Boolean
isValid = todo

getVouchers :: { body :: { signatureObj :: SignatureObj } } -> Aff (Either Failure (Array Voucher))
getVouchers { body: { signatureObj } } = do
  case W3A.recover signatureObj of
    Left _ -> pure $ Left ?a
    Right address -> do
      valid <- isValid signatureObj
      if valid then
        M.lookup (Address $ C.Address address) db # fold # Right # pure
      else pure $ Left ?b

--------------------------------------------------------------------------------
main :: Effect Unit
main = Payload.launch spec
  { getVouchers
  }

