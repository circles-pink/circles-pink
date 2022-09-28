module CirclesSimple.Data.Graph.Diff where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustNode (TrustNode)
import Data.Graph as G
import Data.Graph.Diff as D
import Data.Pair (Pair)

getDiff :: G.Graph Address (Pair Address) TrustNode -> G.Graph Address (Pair Address) TrustNode -> D.GraphDiff Address (Pair Address) TrustNode
getDiff = D.getEdgesDiff <> D.getNodesDiff