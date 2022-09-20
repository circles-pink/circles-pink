module CirclesPink.Data.TrustConnection where

import CirclesPink.Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustState (TrustState)
import Data.Generic.Rep (class Generic)
import Data.IxGraph (class Indexed)
import Data.Pair (Pair)
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

unTrustConnection :: forall z. (Pair Address -> TrustState -> z) -> TrustConnection -> z
unTrustConnection onTrustConnection (TrustConnection x y) = onTrustConnection x y