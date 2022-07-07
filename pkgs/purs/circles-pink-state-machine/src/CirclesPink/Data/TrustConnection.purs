module CirclesPink.Data.TrustConnection where


import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustState (TrustState)
import Data.IxGraph (class Indexed)
import Data.Pair (Pair)


data TrustConnection = TrustConnection (Pair Address) TrustState

instance indexed :: Indexed (Pair Address) TrustConnection where
  getIndex (TrustConnection conn _) = conn