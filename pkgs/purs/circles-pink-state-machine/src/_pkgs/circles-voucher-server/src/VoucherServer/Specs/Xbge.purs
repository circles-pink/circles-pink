module VoucherServer.Specs.Xbge where

import Prelude

import Payload.Spec (POST, Spec(..), GET)
import VoucherServer.Types (SafeAddress, VoucherAmount, VoucherEncrypted, VoucherProvider, VoucherProviderId)

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