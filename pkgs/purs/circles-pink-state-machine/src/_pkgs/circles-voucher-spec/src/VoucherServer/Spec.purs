module VoucherServer.Spec
  ( ErrGetVoucher
  , spec
  ) where

import Payload.Spec (POST, Spec(Spec), GET)
import VoucherServer.Types (Voucher, VoucherProvider)
import Web3 (SignatureObj)

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
           GET "/voucher-providers"
             { response ::
                 Array VoucherProvider
             }
       }
spec = Spec
