module VoucherServer.Types where


import Data.DateTime.Instant (Instant)
import Data.Newtype (class Newtype)
import VoucherServer.Spec.Types (Freckles, TransferId)
import VoucherServer.Specs.Xbge (Address)



newtype Transfer = Transfer
  { from :: Address
  , to :: Address
  , id :: TransferId
  , amount :: Freckles
  }


newtype TransferMeta = TransferMeta
  { id :: String
  , transactionHash :: String
  , time :: Instant
  }

derive instance newtypeTransferMeta :: Newtype TransferMeta _
