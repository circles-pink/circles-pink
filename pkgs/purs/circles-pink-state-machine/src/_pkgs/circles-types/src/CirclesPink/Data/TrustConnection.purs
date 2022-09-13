module CirclesPink.Data.TrustConnection where

import CirclesPink.Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustState (TrustState)
import Data.FpTs.Pair as FpTs
import Data.Generic.Rep (class Generic)
import Data.IxGraph (class Indexed)
import Data.Pair (Pair)
import FpTs.Class (class FpTs, fromFpTs, toFpTs)
import PursTsGen (class ToTsDef, class ToTsType)
import PursTsGen.Class.ToPursType (class ToPursType)

data TrustConnection = TrustConnection (Pair Address) TrustState

derive instance Eq TrustConnection
derive instance Ord TrustConnection

instance Indexed (Pair Address) TrustConnection where
  getIndex (TrustConnection conn _) = conn

derive instance Generic TrustConnection _

instance ToPursNominal TrustConnection where
  toPursNominal _ = PursNominal "CirclesPink.Data.TrustConnection" "TrustConnection"

instance ToTsType TrustConnection where
  toTsType = defaultToTsType' []

instance ToTsDef TrustConnection where
  toTsDef = defaultToTsDef' []

instance ToPursType TrustConnection where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

newtype TsTrustConnection = TsTrustConnection { conn :: FpTs.Pair Address, trustState :: TrustState }

instance fpTs :: FpTs TrustConnection TsTrustConnection where
  toFpTs (TrustConnection conn trustState) = TsTrustConnection { conn: toFpTs conn, trustState }
  fromFpTs (TsTrustConnection { conn, trustState }) = TrustConnection (fromFpTs conn) trustState

--------------------------------------------------------------------------------

unTrustConnection :: forall z. (Pair Address -> TrustState -> z) -> TrustConnection -> z
unTrustConnection onTrustConnection (TrustConnection x y) = onTrustConnection x y