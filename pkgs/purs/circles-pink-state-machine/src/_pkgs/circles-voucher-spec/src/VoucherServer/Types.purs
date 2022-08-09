module VoucherServer.Types where

import Prelude

import Data.BN (BN)
import Data.DateTime.Instant as DT
import Data.Newtype (class Newtype, un)
import Data.Time.Duration (Milliseconds(..))
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

--------------------------------------------------------------------------------
newtype Instant = Instant DT.Instant

derive instance newtypeInstant :: Newtype Instant _

instance writeForeignInstant :: WriteForeign Instant where
  writeImpl = un Instant >>> DT.unInstant >>> un Milliseconds >>> writeImpl

--------------------------------------------------------------------------------

newtype VoucherProvider = VoucherProvider
  { id :: VoucherProviderId
  , name :: String
  , logoUrl :: String
  , shopUrl :: String
  , availableOffers :: Array VoucherOffer
  }

derive newtype instance writeForeignVoucherProvider :: WriteForeign VoucherProvider
derive newtype instance readForeignVoucherProvider :: ReadForeign VoucherProvider

--------------------------------------------------------------------------------

newtype VoucherOffer = VoucherOffer
  { amount :: VoucherAmount
  , countAvailable :: Int
  }

derive newtype instance writeForeignVoucherOffer :: WriteForeign VoucherOffer
derive newtype instance readForeignVoucherOffer :: ReadForeign VoucherOffer

--------------------------------------------------------------------------------

newtype VoucherProviderId = VoucherProviderId String

derive newtype instance writeForeignVoucherProviderId :: WriteForeign VoucherProviderId
derive newtype instance readForeignVoucherProviderId :: ReadForeign VoucherProviderId

--------------------------------------------------------------------------------

newtype EurCent = EurCent Int

derive newtype instance writeForeignEurCent :: WriteForeign EurCent
derive newtype instance readForeignEurCent :: ReadForeign EurCent

--------------------------------------------------------------------------------
newtype VoucherAmount = VoucherAmount EurCent

derive newtype instance writeForeignVoucherAmount :: WriteForeign VoucherAmount
derive newtype instance readForeignVoucherAmount :: ReadForeign VoucherAmount

--------------------------------------------------------------------------------

newtype Frackles = Frackles BN

--------------------------------------------------------------------------------
newtype VoucherCodeEncrypted = VoucherCodeEncrypted String

derive newtype instance readForeignVoucherCodeEncrypted :: ReadForeign VoucherCodeEncrypted

--------------------------------------------------------------------------------

newtype VoucherCode = VoucherCode String

derive newtype instance readForeignVoucherCode :: ReadForeign VoucherCode
derive newtype instance writeForeignVoucherCode :: WriteForeign VoucherCode

--------------------------------------------------------------------------------

newtype VoucherEncrypted =
  VoucherEncrypted
    { voucherProviderId :: VoucherProviderId
    , voucherAmount :: VoucherAmount
    , voucherCode :: VoucherCodeEncrypted
    , sold ::
        { transactionId :: String
        , safeAddress :: String
        , timestamp :: String
        }
    }

derive newtype instance readForeignVoucherEncrypted :: ReadForeign VoucherEncrypted

--------------------------------------------------------------------------------

newtype Voucher =
  Voucher
    { voucherProviderId :: VoucherProviderId
    , voucherAmount :: VoucherAmount
    , voucherCode :: VoucherCode
    , sold ::
        { transactionId :: String
        , safeAddress :: String
        , timestamp :: String
        }
    }

derive newtype instance readForeignVoucher :: ReadForeign Voucher
derive newtype instance writeForeignVoucher :: WriteForeign Voucher