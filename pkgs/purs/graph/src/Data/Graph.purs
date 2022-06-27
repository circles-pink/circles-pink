module Data.Graph
  ( deleteNodes
  , edgeIds
  , foldrEdges
  , foldrNodes
  , fromFoldables
  , insertEdges
  , insertNodes
  , maybeInsertEdge
  , memberEdge
  , memberNode
  , module Exp
  , outgoingNodes
  , toUnfoldables
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Graph.Core (Graph, deleteEdge, deleteNode, empty, incomingIds, insertEdge, insertNode, lookupEdge, lookupNode, outgoingIds) as Exp
import Data.Graph.Core (Graph)
import Data.Graph.Core as G
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)

fromFoldables :: forall f id e n. Ord id => Foldable f => f (id /\ n) -> f (id /\ id /\ e) -> Graph id e n
fromFoldables nodes edges = G.empty
  # (\g -> foldr (\(id /\ node) -> G.insertNode id node) g nodes)
  # (\g -> foldr (\(from /\ to /\ edge) -> G.insertEdge from to edge) g edges)

memberNode :: forall id e n. Ord id => id -> Graph id e n -> Boolean
memberNode id g = isJust $ G.lookupNode id g

memberEdge :: forall id e n. Ord id => id -> id -> Graph id e n -> Boolean
memberEdge from to g = isJust $ G.lookupEdge from to g

maybeInsertEdge :: forall id e n. Ord id => id -> id -> e -> Graph id e n -> Maybe (Graph id e n)
maybeInsertEdge from _ _ g | not (memberNode from g) = Nothing
maybeInsertEdge _ to _ g | not (memberNode to g) = Nothing
maybeInsertEdge from to edge g = Just $ G.insertEdge from to edge g

deleteNodes :: forall f id e n. Ord id => Foldable f => f id -> Graph id e n -> Graph id e n
deleteNodes ids g = foldr G.deleteNode g ids

insertEdges :: forall f id e n. Ord id => Foldable f => f (id /\ id /\ e) -> Graph id e n -> Graph id e n
insertEdges edges g = foldr (\(from /\ to /\ edge) -> G.insertEdge from to edge) g edges

insertNodes :: forall f id e n. Foldable f => Ord id => f (id /\ n) -> Graph id e n -> Graph id e n
insertNodes nodes g = foldr (\(id /\ node) -> G.insertNode id node) g nodes

outgoingNodes :: forall id e n. Ord id => Ord n => id -> Graph id e n -> Maybe (Set n)
outgoingNodes id graph = G.outgoingIds id graph <#> S.map (\id' -> unsafePartial $ unsafeLookupNode id' graph)

edgeIds :: forall id e n. Ord id => Graph id e n -> Set (id /\ id)
edgeIds g = g
  # G.nodeIds
  # S.map (\from -> G.outgoingIds from g # maybe mempty identity # S.map (\to -> from /\ to))
  # S.unions

nodes :: forall id e n. Ord id => Ord n => Graph id e n -> Set n
nodes g = g # G.nodeIds # S.map (\id -> unsafePartial $ unsafeLookupNode id g)

edges :: forall id e n. Ord id => Ord e => Graph id e n -> Set e
edges g = g # edgeIds # S.map (\(from /\ to) -> unsafePartial $ unsafeLookupEdge from to g)

foldrEdges :: forall id e n z. Ord id => Ord e => (e -> z -> z) -> z -> Graph id e n -> z
foldrEdges f x g = edges g # foldr f x

foldrNodes :: forall id e n z. Ord id => Ord n => (n -> z -> z) -> z -> Graph id e n -> z
foldrNodes f x g = nodes g # foldr f x

toUnfoldables :: forall id e n f. Unfoldable f => Ord id => Graph id e n -> { nodes :: f (id /\ n), edges :: f (id /\ id /\ e) }
toUnfoldables g = { nodes: G.nodesToUnfoldable g, edges: G.edgesToUnfoldable g }

--------------------------------------------------------------------------------

unsafeLookupNode :: forall id e n. Partial => Ord id => id -> Graph id e n -> n
unsafeLookupNode id' g = fromJust $ G.lookupNode id' g

unsafeLookupEdge :: forall id e n. Partial => Ord id => id -> id -> Graph id e n -> e
unsafeLookupEdge from to g = fromJust $ G.lookupEdge from to g