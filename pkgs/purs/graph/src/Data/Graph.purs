module Data.Graph
  ( deleteEdges
  , deleteNodes
  , edgeIds
  , edges
  , fromFoldables
  , incomingEdgesWithNodes
  , insertEdges
  , insertNodes
  , maybeInsertEdge
  , memberEdge
  , memberNode
  , module Exp
  , neighborEdgesWithNodes
  , neighborIds
  , nodes
  , outgoingEdgesWithNodes
  , outgoingNodes
  , toUnfoldables
  ) where

import Prelude

import Data.Foldable (class Foldable, fold, foldr)
import Data.Graph.Core (Graph)
import Data.Graph.Core (Graph, deleteEdge, deleteNode, empty, incomingIds, insertEdge, insertNode, lookupEdge, lookupNode, outgoingIds) as Exp
import Data.Graph.Core as G
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Pair (Pair, (~))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)

type IxNode id n = id /\ n
type IxEdge id e = Pair id /\ e

type IxEdgeWithNode id e n = IxEdge id e /\ IxNode id n

fromFoldables :: forall f id e n. Ord id => Foldable f => f (id /\ n) -> f (Pair id /\ e) -> Graph id e n
fromFoldables nodes' edges' = G.empty
  # (\g -> foldr (\(id /\ node) -> G.insertNode id node) g nodes')
  # (\g -> foldr (\(conn /\ edge) -> G.insertEdge conn edge) g edges')

memberNode :: forall id e n. Ord id => id -> Graph id e n -> Boolean
memberNode id g = isJust $ G.lookupNode id g

memberEdge :: forall id e n. Ord id => Pair id -> Graph id e n -> Boolean
memberEdge conn g = isJust $ G.lookupEdge conn g

maybeInsertEdge :: forall id e n. Ord id => Pair id -> e -> Graph id e n -> Maybe (Graph id e n)
maybeInsertEdge (from ~ _) _ g | not (memberNode from g) = Nothing
maybeInsertEdge (_ ~ to) _ g | not (memberNode to g) = Nothing
maybeInsertEdge conn edge g = Just $ G.insertEdge conn edge g

deleteNodes :: forall f id e n. Ord id => Foldable f => f id -> Graph id e n -> Graph id e n
deleteNodes ids g = foldr G.deleteNode g ids

deleteEdges :: forall f id e n. Ord id => Foldable f => f (Pair id) -> Graph id e n -> Graph id e n
deleteEdges ids g = foldr G.deleteEdge g ids

insertEdges :: forall f id e n. Ord id => Foldable f => f (Pair id /\ e) -> Graph id e n -> Graph id e n
insertEdges edges' g = foldr (\(conn /\ edge) -> G.insertEdge conn edge) g edges'

insertNodes :: forall f id e n. Foldable f => Ord id => f (id /\ n) -> Graph id e n -> Graph id e n
insertNodes nodes' g = foldr (\(id /\ node) -> G.insertNode id node) g nodes'

outgoingNodes :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Array (id /\ n))
outgoingNodes id graph = graph
  # G.outgoingIds id
  <#> S.toUnfoldable
  <#> map (\id' -> id' /\ (unsafePartial $ partLookupNode id' graph))

edgeIds :: forall id e n. Ord id => Graph id e n -> Set (Pair id)
edgeIds g = g
  # G.nodeIds
  # S.map (\from -> G.outgoingIds from g # fold # S.map (\to -> from ~ to))
  # S.unions

nodes :: forall id e n. Ord id => Graph id e n -> Array (id /\ n)
nodes g = g # G.nodeIds # S.toUnfoldable <#> (\id -> id /\ (unsafePartial $ partLookupNode id g))

edges :: forall id e n. Ord id => Graph id e n -> Array (Pair id /\ e)
edges g = g # edgeIds # S.toUnfoldable <#>  (\conn -> conn /\ (unsafePartial $ partLookupEdge conn g))

toUnfoldables :: forall id e n f. Unfoldable f => Ord id => Graph id e n -> { nodes :: f (id /\ n), edges :: f (Pair id /\ e) }
toUnfoldables g = { nodes: G.nodesToUnfoldable g, edges: G.edgesToUnfoldable g }

neighborIds :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Set id)
neighborIds id graph = (<>) <$> G.outgoingIds id graph <*> G.incomingIds id graph

outgoingEdgesWithNodes :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Array (IxEdgeWithNode id e n))
outgoingEdgesWithNodes fromId graph = do
  ids <- G.outgoingIds fromId graph
  ids # S.toUnfoldable <#> getBoth # pure
  where
  getBoth toId = (unsafePartial $ partLookupEdgeIx (fromId ~ toId) graph) /\
    (unsafePartial $ partLookupNodeIx toId graph)

incomingEdgesWithNodes :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Array (IxEdgeWithNode id e n))
incomingEdgesWithNodes fromId graph = do
  ids <- G.incomingIds fromId graph
  ids # S.toUnfoldable <#> getBoth # pure
  where
  getBoth toId = (unsafePartial $ partLookupEdgeIx (toId ~ fromId) graph) /\
    (unsafePartial $ partLookupNodeIx toId graph)

neighborEdgesWithNodes :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Array (IxEdgeWithNode id e n))
neighborEdgesWithNodes id g = (<>) <$> incomingEdgesWithNodes id g <*> outgoingEdgesWithNodes id g

--------------------------------------------------------------------------------

partLookupNode :: forall id e n. Partial => Ord id => id -> Graph id e n -> n
partLookupNode id' g = fromJust $ G.lookupNode id' g

partLookupNodeIx :: forall id e n. Partial => Ord id => id -> Graph id e n -> IxNode id n
partLookupNodeIx id' g = id' /\ (fromJust $ G.lookupNode id' g)

partLookupEdge :: forall id e n. Partial => Ord id => Pair id -> Graph id e n -> e
partLookupEdge conn g = fromJust $ G.lookupEdge conn g

partLookupEdgeIx :: forall id e n. Partial => Ord id => Pair id -> Graph id e n -> IxEdge id e
partLookupEdgeIx conn g = conn /\ (fromJust $ G.lookupEdge conn g)
