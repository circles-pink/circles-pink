module VoucherServer.Spec
  ( ErrGetVoucher
  , VoucherServerSpec
  , spec
  ) where

import CirclesPink.Data.Address (Address)
import Payload.Spec (POST, Spec(Spec), GET)
import VoucherServer.Types (Voucher, VoucherProvider)
import Web3 (SignatureObj)

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------

type VoucherServerSpec = Spec
  { getVouchers ::
      POST "/vouchers"
        { body :: { signatureObj :: SignatureObj }
        , response :: Array Voucher
        }
  , getVoucherProviders ::
      GET "/voucher-providers"
        { response ::
            Array VoucherProvider
        }
  , trustUsers ::
      POST "/trust-users"
        { response :: {}
        , body :: { safeAddresses :: Array Address }
        }
  }

spec :: VoucherServerSpec
spec = Spec
