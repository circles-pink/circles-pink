module VoucherServer.Spec
  ( Address(..)
  , ErrGetVoucher
  , Instant(..)
  , spec
  ) where

import Prelude

import CirclesPink.Data.Address (parseAddress)
import CirclesPink.Data.Address as C
import Data.DateTime.Instant as DT
import Data.Either (note)
import Data.Newtype (class Newtype, un)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Milliseconds(..))
import Payload.Server.Params (class DecodeParam, decodeParam)
import Payload.Spec (POST, Spec(Spec), GET)
import Simple.JSON (class WriteForeign, writeImpl)
import VoucherServer.Types (Voucher(..), VoucherAmount(..), VoucherMeta(..), VoucherProvider(..))
import Web3 (SignatureObj)

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

--------------------------------------------------------------------------------
spec
  :: Spec
       { getVouchers ::
           POST "/vouchers"
             { body :: { signatureObj :: SignatureObj }
             , response :: Array Voucher
             }
       , getVoucherProviders ::
           GET "/get-voucher-providers"
             { response ::
                 Array
                   ( VoucherProvider /\
                       Array (VoucherAmount /\ VoucherMeta)
                   )
             }
       }
spec = Spec
