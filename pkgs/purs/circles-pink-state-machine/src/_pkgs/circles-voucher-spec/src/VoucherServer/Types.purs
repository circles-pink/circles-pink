module VoucherServer.Types where

import Data.BN (BN)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype VoucherProvider = VoucherProvider
  { id :: VoucherProviderId
  , name :: String
  , logoUrl :: String
  , shopUrl :: String
  , availableOffers :: Array VoucherOffer
  }

derive newtype instance writeForeignVoucherProvider :: WriteForeign VoucherProvider
derive newtype instance readForeignVoucherProvider :: ReadForeign VoucherProvider

newtype VoucherOffer = VoucherOffer
  { amount :: VoucherAmount
  , countAvailable :: Int
  }

derive newtype instance writeForeignVoucherOffer :: WriteForeign VoucherOffer
derive newtype instance readForeignVoucherOffer :: ReadForeign VoucherOffer

newtype VoucherProviderId = VoucherProviderId String

derive newtype instance writeForeignVoucherProviderId :: WriteForeign VoucherProviderId
derive newtype instance readForeignVoucherProviderId :: ReadForeign VoucherProviderId

newtype EurCent = EurCent Int

derive newtype instance writeForeignEurCent :: WriteForeign EurCent
derive newtype instance readForeignEurCent :: ReadForeign EurCent

newtype VoucherAmount = VoucherAmount EurCent

derive newtype instance writeForeignVoucherAmount :: WriteForeign VoucherAmount
derive newtype instance readForeignVoucherAmount :: ReadForeign VoucherAmount

newtype Frackles = Frackles BN

newtype VoucherCodeEncrypted = VoucherCodeEncrypted String

newtype VoucherCode = VoucherCode String

newtype VoucherEncrypted = VoucherEncrypted
  { voucherProviderId :: VoucherProviderId
  , voucherCode :: VoucherCodeEncrypted
  }

newtype Voucher =
  Voucher
    { voucherProviderId :: VoucherProviderId
    , voucherCode :: VoucherCode
    }