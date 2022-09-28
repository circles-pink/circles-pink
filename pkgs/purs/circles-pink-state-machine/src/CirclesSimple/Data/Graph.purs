module CirclesSimple.Data.Graph where

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustConnection (TrustConnection)
import CirclesPink.Data.TrustNode (TrustNode)
import Data.Graph as G

type CirclesGraph' = G.Graph Address TrustConnection TrustNode

empty :: CirclesGraph'
empty = G.empty