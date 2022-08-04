module VoucherServer.Specs.Xbge where

import Prelude

import CirclesPink.Data.SafeAddress (SafeAddress)
import Payload.Spec (POST, Spec(..), GET)
import VoucherServer.Types (VoucherAmount, VoucherEncrypted, VoucherProvider, VoucherProviderId)

xbgeSpec
  :: Spec
       { finalizeVoucherPurchase ::
           POST "/finalize-voucher-purchase"
             { body ::
                 { safeAddress :: SafeAddress
                 , voucherProviderId :: VoucherProviderId
                 , voucherAmount :: VoucherAmount
                 }
             , response :: Unit
             }
       , getVoucherProviders ::
           GET "/voucher-providers"
             { response :: Array VoucherProvider
             }
       , getVouchersOfUser ::
           GET "/vouchers-of-user/<safeAddress>"
             { params :: { safeAddress :: SafeAddress }
             , response :: Array VoucherEncrypted
             }
       }
xbgeSpec = Spec