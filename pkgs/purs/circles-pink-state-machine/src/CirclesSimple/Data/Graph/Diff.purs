module CirclesSimple.Data.Graph.Diff where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustConnection (TrustConnection)
import CirclesPink.Data.TrustNode (TrustNode)
import CirclesSimple.Data.Graph (CirclesGraph')
import Data.Graph as G
import Data.Graph.Diff as D
import Data.Map (fromFoldable)
import Data.Newtype (unwrap)

getDiff :: CirclesGraph' -> CirclesGraph' -> D.GraphDiff Address TrustConnection TrustNode
getDiff = D.getDiff

isStructuralChange :: CirclesGraph' -> CirclesGraph' -> Boolean
isStructuralChange = D.isStructuralChange

rootHasChanged :: CirclesGraph' -> CirclesGraph' -> Boolean
rootHasChanged g1 g2 =
  let
    g1Roots = G.nodes g1 # fromFoldable <#> unwrap >>> _.root
    g2Roots = G.nodes g2 # fromFoldable <#> unwrap >>> _.root
  in
    g1Roots /= g2Roots