module VoucherServer.Specs.Xbge where

import Prelude

import CirclesPink.Data.SafeAddress as C
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Payload.Client.EncodeParam (class EncodeParam, encodeParam)
import Payload.Client.QueryParams (class EncodeQueryParam, encodeQueryParam)
import Payload.Spec (POST, Spec(..), GET)
import Simple.JSON (class WriteForeign)
import VoucherServer.Types (VoucherAmount, VoucherEncrypted, VoucherProvider, VoucherProviderId)

newtype SafeAddress = SafeAddress C.SafeAddress

derive instance newtypeSafeAddress :: Newtype SafeAddress _

derive newtype instance ordSafeAddress :: Ord SafeAddress
derive newtype instance eqSafeAddress :: Eq SafeAddress
derive newtype instance showSafeAddress :: Show SafeAddress
derive newtype instance writeForeignSafeAddress :: WriteForeign SafeAddress

-- instance decodeParamSafeAddress :: DecodeParam SafeAddress where
--   decodeParam x = decodeParam x
--     >>= (parseAddress >>> note "Could not parse SafeAddress")
--     <#> SafeAddress

instance encodeParamSafeAddress :: EncodeParam SafeAddress where
  encodeParam (SafeAddress x) = encodeParam $ show x

instance encodeQueryParamSafeAddress :: EncodeQueryParam SafeAddress where
  encodeQueryParam (SafeAddress x) = encodeQueryParam $ show x

xbgeSpec
  :: Spec
       { finalizeVoucherPurchase ::
           POST "/finalize-voucher-purchase"
             { body ::
                 { safeAddress :: SafeAddress
                 , providerId :: VoucherProviderId
                 , amount :: VoucherAmount
                 }
             , response :: Unit
             }
       , getVoucherProviders ::
           GET "/voucher-providers"
             { response :: {data :: Array VoucherProvider}
             }
       , getVouchers ::
           GET "/vouchers?safeAddress=<safeAddress>"
             { query :: { safeAddress :: Maybe SafeAddress }
             , response :: { data :: Array VoucherEncrypted }
             }
       }
xbgeSpec = Spec