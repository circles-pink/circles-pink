module VoucherServer.Types where

import Data.BN (BN)
import Network.Ethereum.Core.Signatures (Address)

newtype SafeAddress = SafeAddress Address
newtype VoucherProvider = VoucherProvider
  { id :: VoucherProviderId
  , name :: String
  , logoUrl :: String
  , shopUrl :: String
  , availableOffers :: Array VoucherOffer
  }

newtype VoucherOffer = VoucherOffer
  { amount :: VoucherAmount
  , countAvailable :: Int
  }

newtype VoucherProviderId = VoucherProviderId String

newtype EurCent = EurCent Int

newtype VoucherAmount = VoucherAmount EurCent

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