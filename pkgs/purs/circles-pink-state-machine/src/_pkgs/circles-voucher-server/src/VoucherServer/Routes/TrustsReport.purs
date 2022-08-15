module VoucherServer.Routes.TrustsReport
  ( trustsReport
  ) where

import Prelude

import CirclesPink.Data.Address (Address)
import VoucherServer.MonadVoucherServer (class MonadVoucherServer)

trustsReport
  :: forall m
   . MonadVoucherServer m
  => { body :: { addresses :: Array Address } }
  -> m { trusted :: Array Address, notTrusted :: Array Address }
trustsReport {} =
  pure { trusted: [], notTrusted: [] }