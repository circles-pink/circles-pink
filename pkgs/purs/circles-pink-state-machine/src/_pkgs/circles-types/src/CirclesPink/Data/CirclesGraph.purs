module CirclesPink.Data.CirclesGraph where


import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustConnection (TrustConnection)
import CirclesPink.Data.TrustNode (TrustNode)
import Data.IxGraph (IxGraph)


type CirclesGraph = IxGraph Address TrustConnection TrustNode