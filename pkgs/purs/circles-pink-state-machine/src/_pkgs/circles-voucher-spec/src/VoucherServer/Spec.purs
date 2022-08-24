module VoucherServer.Spec
  ( ErrGetVoucher
  , VoucherServerSpec
  , spec
  ) where

import Prelude

import CirclesPink.Data.Address (Address)
import Payload.Spec (type (:), GET, Guards, POST, Spec(Spec), Nil)
import VoucherServer.Spec.Types (Voucher, VoucherProvider)
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
      , trustUsers ::
          POST "/trust-users"
            { response :: {}
            , body :: { safeAddresses :: Array Address }
            , guards :: Guards ("basicAuth" : Nil)
            }
      , trustsReport ::
          POST "/trusts-report"
            { body :: { safeAddresses :: Array Address }
            , response ::
                { trusted :: Array Address
                , notTrusted :: Array Address
                }
            , guards :: Guards ("basicAuth" : Nil)
            }
      , trustCount ::
          POST "/trust-count"
            { body :: { safeAddresses :: Array Address }
            , response ::
                Array { safeAddress :: Address, trustConnections :: Int }
            , guards :: Guards ("basicAuth" : Nil)
            }
      }
  }

spec :: VoucherServerSpec
spec = Spec
