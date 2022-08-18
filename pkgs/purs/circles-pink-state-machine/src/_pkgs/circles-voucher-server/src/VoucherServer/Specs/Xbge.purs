module VoucherServer.Specs.Xbge where

import Prelude

import CirclesPink.Data.Address as C
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Payload.Client.EncodeParam (class EncodeParam, encodeParam)
import Payload.Client.QueryParams (class EncodeQueryParam, encodeQueryParam)
import Payload.Spec (POST, Spec(..), GET)
import Simple.JSON (class WriteForeign)
import Test.QuickCheck (class Arbitrary)
import TypedEnv (class ParseValue)
import VoucherServer.Spec.Types (TransferId, VoucherAmount, VoucherEncrypted, VoucherProvider, VoucherProviderId)

--------------------------------------------------------------------------------
-- Payload Spec
--------------------------------------------------------------------------------

xbgeSpec
  :: Spec
       { finalizeVoucherPurchase ::
           POST "/vouchers"
             { body ::
                 { safeAddress :: Address
                 , providerId :: VoucherProviderId
                 , amount :: VoucherAmount
                 , transactionId :: TransferId
                 }
             , response :: { data :: VoucherEncrypted }
             }
       , getVoucherProviders ::
           GET "/vouchers/providers"
             { response :: { data :: Array VoucherProvider }
             }
       , getVouchers ::
           GET "/vouchers?safeAddress=<safeAddress>"
             { query :: { safeAddress :: Maybe Address }
             , response :: { data :: Array VoucherEncrypted }
             }
       }
xbgeSpec = Spec

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Address = Address C.Address

derive instance newtypeAddress :: Newtype Address _

derive newtype instance ordAddress :: Ord Address
derive newtype instance eqAddress :: Eq Address
derive newtype instance showAddress :: Show Address
derive newtype instance writeForeignAddress :: WriteForeign Address
derive newtype instance parseValueAddress :: ParseValue Address

-- instance decodeParamAddress :: DecodeParam Address where
--   decodeParam x = decodeParam x
--     >>= (parseAddress >>> note "Could not parse Address")
--     <#> Address

instance encodeParamAddress :: EncodeParam Address where
  encodeParam (Address x) = encodeParam $ show x

instance encodeQueryParamAddress :: EncodeQueryParam Address where
  encodeQueryParam (Address x) = encodeQueryParam $ show x

instance arbitraryAddress :: Arbitrary Address where
  arbitrary = pure sampleAddress

sampleAddress :: Address
sampleAddress = Address C.sampleAddress
