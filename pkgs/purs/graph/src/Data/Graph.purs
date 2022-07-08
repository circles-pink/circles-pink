module Data.Graph
  ( edgeIds
  , edges
  , incomingEdgesWithNodes
  , module Exp
  , neighborEdgesWithNodes
  , neighborIds
  , nodes
  , outgoingEdgesWithNodes
  , outgoingNodes
  ) where

import Prelude

import Data.Either (fromRight')
import Data.Foldable (fold)
import Data.Graph.Core (EitherV, Graph)
import Data.Graph.Core as C
import Data.Graph.Core (GraphSpec, fromFoldables, EitherV, Graph, addEdge, addNode, deleteEdge, deleteNode, edgesToUnfoldable, empty, foldMapWithIndex, foldlWithIndex, foldrWithIndex, incomingIds, lookupEdge, lookupNode, memberEdge, memberNode, nodeIds, nodesToUnfoldable, outgoingIds, updateEdge, updateNode) as Exp
import Data.Graph.Errors (ErrIncomingEdgesWithNodes, ErrNeighborEdgesWithNodes, ErrNeighborIds, ErrOutgoingEdgesWithNodes, ErrOutgoingNodes)
import Data.Pair (Pair, (~))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\), (/\))
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

type IxNode id n = id /\ n
type IxEdge id e = Pair id /\ e

type IxEdgeWithNode id e n = IxEdge id e /\ IxNode id n

--------------------------------------------------------------------------------
-- Graph API
--------------------------------------------------------------------------------

nodes :: forall id e n. Ord id => Graph id e n -> Array (id /\ n)
nodes g = g # C.nodeIds # S.toUnfoldable <#> (\id -> id /\ (unsafePartial $ partLookupNode id g))

edges :: forall id e n. Ord id => Graph id e n -> Array (Pair id /\ e)
edges g = g # edgeIds # S.toUnfoldable <#> (\conn -> conn /\ (unsafePartial $ partLookupEdge conn g))

--------------------------------------------------------------------------------
-- Node API
--------------------------------------------------------------------------------

outgoingNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrOutgoingNodes id r) (Array (id /\ n))
outgoingNodes id graph = graph
  # C.outgoingIds id
  <#> S.toUnfoldable
  <#> map (\id' -> id' /\ (unsafePartial $ partLookupNode id' graph))

--------------------------------------------------------------------------------
-- Edge API
--------------------------------------------------------------------------------

edgeIds :: forall id e n. Ord id => Graph id e n -> Set (Pair id)
edgeIds g = g
  # C.nodeIds
  # S.map (\from -> C.outgoingIds from g # fold # S.map (\to -> from ~ to))
  # S.unions

--------------------------------------------------------------------------------

neighborIds :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrNeighborIds id r) (Set id)
neighborIds id graph = (<>) <$> C.outgoingIds id graph <*> C.incomingIds id graph

outgoingEdgesWithNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrOutgoingEdgesWithNodes id r) (Array (IxEdgeWithNode id e n))
outgoingEdgesWithNodes fromId graph = do
  ids <- C.outgoingIds fromId graph
  ids # S.toUnfoldable <#> getBoth # pure
  where
  getBoth toId = (unsafePartial $ partLookupEdgeIx (fromId ~ toId) graph) /\
    (unsafePartial $ partLookupNodeIx toId graph)

incomingEdgesWithNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrIncomingEdgesWithNodes id r) (Array (IxEdgeWithNode id e n))
incomingEdgesWithNodes fromId graph = do
  ids <- C.incomingIds fromId graph
  ids # S.toUnfoldable <#> getBoth # pure
  where
  getBoth toId = (unsafePartial $ partLookupEdgeIx (toId ~ fromId) graph) /\
    (unsafePartial $ partLookupNodeIx toId graph)

neighborEdgesWithNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrNeighborEdgesWithNodes id r) (Array (IxEdgeWithNode id e n))
neighborEdgesWithNodes id g = (<>) <$> incomingEdgesWithNodes id g <*> outgoingEdgesWithNodes id g

--------------------------------------------------------------------------------

partLookupNode :: forall id e n. Partial => Ord id => id -> Graph id e n -> n
partLookupNode id' g = fromRight' (crashWith "Inconsistency: Node not found") $ C.lookupNode id' g

partLookupNodeIx :: forall id e n. Partial => Ord id => id -> Graph id e n -> IxNode id n
partLookupNodeIx id' g = id' /\ partLookupNode id' g

partLookupEdge :: forall id e n. Partial => Ord id => Pair id -> Graph id e n -> e
partLookupEdge conn g = fromRight' (crashWith "Inconsistency: Edge not found") $ C.lookupEdge conn g

partLookupEdgeIx :: forall id e n. Partial => Ord id => Pair id -> Graph id e n -> IxEdge id e
partLookupEdgeIx conn g = conn /\ partLookupEdge conn g
