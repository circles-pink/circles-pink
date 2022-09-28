module CirclesSimple.Data.Graph.Diff where

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustConnection (TrustConnection)
import CirclesPink.Data.TrustNode (TrustNode)
import CirclesSimple.Data.Graph (CirclesGraph')
import Data.Graph.Diff as D

getDiff :: CirclesGraph' -> CirclesGraph' -> D.GraphDiff Address TrustConnection TrustNode
getDiff = D.getDiff