module CirclesSimple.Data.Graph where

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustNode (TrustNode)
import Data.Graph as G
import Data.Pair (Pair)

empty :: G.Graph Address (Pair Address) TrustNode
empty = G.empty