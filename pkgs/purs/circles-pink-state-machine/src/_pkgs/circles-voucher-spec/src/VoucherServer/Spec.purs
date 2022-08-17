module VoucherServer.Spec
  ( ErrGetVoucher
  , VoucherServerSpec
  , spec
  ) where

import Prelude

import CirclesPink.Data.Address (Address)
import Payload.Spec (type (:), GET, Guards, POST, Spec(Spec), Nil)
import VoucherServer.Types (Voucher, VoucherProvider)
import Web3 (SignatureObj)

--------------------------------------------------------------------------------

type ErrGetVoucher = String

--------------------------------------------------------------------------------

type VoucherServerSpec = Spec
  { guards ::
      { basicAuth :: Unit
      }
  , routes ::
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
      -- , trustUsers ::
      --     POST "/trust-users"
      --       { response :: {}
      --       , body :: { safeAddresses :: Array Address }
      --       }
      , trustsReport ::
          POST "/trusts-report"
            { body :: { addresses :: Array Address }
            , response ::
                { trusted :: Array Address
                , notTrusted :: Array Address
                }
           , guards :: Guards ("basicAuth" : Nil)
            }
      }
  }

spec :: VoucherServerSpec
spec = Spec
