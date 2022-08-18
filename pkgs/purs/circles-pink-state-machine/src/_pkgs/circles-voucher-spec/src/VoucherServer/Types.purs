module VoucherServer.Spec.Types where

import Prelude

import Data.BN (BN)
import Data.DateTime.Instant as DT
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
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

derive instance genericVoucherProviderId :: Generic VoucherProviderId _
derive newtype instance writeForeignVoucherProviderId :: WriteForeign VoucherProviderId
derive newtype instance readForeignVoucherProviderId :: ReadForeign VoucherProviderId
derive newtype instance eqVoucherProviderId :: Eq VoucherProviderId
derive newtype instance ordVoucherProviderId :: Ord VoucherProviderId

instance showVoucherProviderId :: Show VoucherProviderId where
  show = genericShow

--------------------------------------------------------------------------------

newtype EurCent = EurCent Int

derive instance genericEurCent :: Generic EurCent _
derive newtype instance writeForeignEurCent :: WriteForeign EurCent
derive newtype instance readForeignEurCent :: ReadForeign EurCent
-- derive newtype instance semiringEurCent :: Semiring EurCent
-- derive newtype instance eqEurCent :: Eq EurCent
-- derive newtype instance ordEurCent :: Ord EurCent

instance showEurCent :: Show EurCent where
  show = genericShow

--------------------------------------------------------------------------------
newtype VoucherAmount = VoucherAmount EurCent

derive instance genericVoucherAmount :: Generic VoucherAmount _
derive newtype instance writeForeignVoucherAmount :: WriteForeign VoucherAmount
derive newtype instance readForeignVoucherAmount :: ReadForeign VoucherAmount

instance showVoucherAmount :: Show VoucherAmount where
  show = genericShow

--------------------------------------------------------------------------------

newtype Freckles = Freckles BN

--------------------------------------------------------------------------------
newtype VoucherCodeEncrypted = VoucherCodeEncrypted String

derive instance genericVoucherCodeEncrypted :: Generic VoucherCodeEncrypted _
derive newtype instance readForeignVoucherCodeEncrypted :: ReadForeign VoucherCodeEncrypted

instance showVoucherCodeEncrypted :: Show VoucherCodeEncrypted where
  show = genericShow

--------------------------------------------------------------------------------

newtype VoucherCode = VoucherCode String

derive newtype instance readForeignVoucherCode :: ReadForeign VoucherCode
derive newtype instance writeForeignVoucherCode :: WriteForeign VoucherCode

--------------------------------------------------------------------------------

newtype VoucherEncrypted =
  VoucherEncrypted
    { providerId :: VoucherProviderId
    , amount :: VoucherAmount
    , code :: VoucherCodeEncrypted
    , sold ::
        { transactionId :: TransferId
        , safeAddress :: String
        , timestamp :: String
        }
    }

derive newtype instance readForeignVoucherEncrypted :: ReadForeign VoucherEncrypted
derive instance genericVoucherEncrypted :: Generic VoucherEncrypted _

instance showVoucherEncrypted :: Show VoucherEncrypted where
  show = genericShow

--------------------------------------------------------------------------------

newtype Voucher =
  Voucher
    { providerId :: VoucherProviderId
    , amount :: VoucherAmount
    , code :: VoucherCode
    , sold ::
        { transactionId :: TransferId
        , safeAddress :: String
        , timestamp :: String
        }
    }

derive newtype instance readForeignVoucher :: ReadForeign Voucher
derive newtype instance writeForeignVoucher :: WriteForeign Voucher

--------------------------------------------------------------------------------

newtype TransferId = TransferId String

derive instance genericTransferId :: Generic TransferId _
derive instance newtypeTransferId :: Newtype TransferId _
derive newtype instance readForeignTransferId :: ReadForeign TransferId
derive newtype instance writeForeignTransferId :: WriteForeign TransferId
derive newtype instance eqTransferId :: Eq TransferId
derive newtype instance ordTransferId :: Ord TransferId

instance showTransferId :: Show TransferId where
  show = genericShow

