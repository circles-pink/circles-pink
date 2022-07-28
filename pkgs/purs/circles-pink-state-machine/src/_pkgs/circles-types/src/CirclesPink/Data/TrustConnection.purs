module CirclesPink.Data.TrustConnection where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustState (TrustState)
import Data.FpTs.Pair as FpTs
import Data.IxGraph (class Indexed)
import Data.Pair (Pair)
import FpTs.Class (class FpTs, fromFpTs, toFpTs)

data TrustConnection = TrustConnection (Pair Address) TrustState

instance indexed :: Indexed (Pair Address) TrustConnection where
  getIndex (TrustConnection conn _) = conn

derive instance eq :: Eq TrustConnection

derive instance ord :: Ord TrustConnection

newtype TsTrustConnection = TsTrustConnection { conn :: FpTs.Pair Address, trustState :: TrustState }

instance fpTs :: FpTs TrustConnection TsTrustConnection where
  toFpTs (TrustConnection conn trustState) = TsTrustConnection { conn: toFpTs conn, trustState }
  fromFpTs (TsTrustConnection { conn, trustState }) = TrustConnection (fromFpTs conn) trustState