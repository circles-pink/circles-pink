module CirclesPink.Data.TrustConnection where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustState (TrustState(..))
import Data.FpTs.Pair as FpTs
import Data.IxGraph (class Indexed)
import Data.Pair (Pair)
import Debug.Extra (todo)
import FpTs.Class (class FpTs, fromFpTs)

data TrustConnection = TrustConnection (Pair Address) TrustState

instance indexed :: Indexed (Pair Address) TrustConnection where
  getIndex (TrustConnection conn _) = conn

derive instance eq :: Eq TrustConnection

derive instance ord :: Ord TrustConnection

instance fpTs :: FpTs TrustConnection { conn :: FpTs.Pair Address, trustState :: TrustState } where
  toFpTs = todo
  fromFpTs = todo