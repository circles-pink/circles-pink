module VoucherServer.Routes.GetVouchers where

import Prelude

import CirclesPink.Data.Address (Address(..))
import Debug.Extra (todo)
import VoucherServer.MonadApp (class MonadApp)
import VoucherServer.Spec.Types (Voucher(..))
import Web3 (SignatureObj(..))


getVouchers :: forall m. MonadApp m => { body :: { signatureObj :: SignatureObj } } -> m (Array Voucher)
getVouchers  _ = todo


authChallenge :: forall m. MonadApp m => m Address
authChallenge = todo