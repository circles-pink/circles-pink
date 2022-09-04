module VoucherServer.Types.AppScope where

import Prelude

import VoucherServer.Types (Transfer(..))

data AppScope
  = AtApp
  | AtSync AppScope
  | AtFinalizeTx Transfer AppScope
  | AtRoute AppScope
  | AtRouteGetVouchers AppScope
  | AtAuthChallenge AppScope

