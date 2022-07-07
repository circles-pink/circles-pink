module Data.Graph
  ( addEdge
  , addNode
  , deleteEdge
  , deleteNode
  , edgeIds
  , edges
  , fromFoldables
  , incomingEdgesWithNodes
  , memberEdge
  , memberNode
  , module Exp
  , neighborEdgesWithNodes
  , neighborIds
  , nodes
  , outgoingEdgesWithNodes
  , outgoingNodes
  , toUnfoldables
  , updateEdge
  , updateNode
  )
  where

import Prelude

import Data.Foldable (class Foldable, fold, foldM)
import Data.Graph.Core (Graph)
import Data.Graph.Core (Graph, empty, incomingIds, lookupEdge, lookupNode, outgoingIds) as Exp
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

--------------------------------------------------------------------------------
-- Graph API
--------------------------------------------------------------------------------

fromFoldables :: forall f id e n. Ord id => Foldable f => { nodes :: f (id /\ n), edges :: f (Pair id /\ e) } -> Maybe (Graph id e n)
fromFoldables { nodes: nodes', edges: edges' } = G.empty
  # (\g -> foldM (\g' (id /\ node) -> addNode id node g') g nodes')
  >>= (\g -> foldM (\g' (conn /\ edge) -> addEdge conn edge g') g edges')

nodes :: forall id e n. Ord id => Graph id e n -> Array (id /\ n)
nodes g = g # G.nodeIds # S.toUnfoldable <#> (\id -> id /\ (unsafePartial $ partLookupNode id g))

edges :: forall id e n. Ord id => Graph id e n -> Array (Pair id /\ e)
edges g = g # edgeIds # S.toUnfoldable <#> (\conn -> conn /\ (unsafePartial $ partLookupEdge conn g))

toUnfoldables :: forall id e n f. Unfoldable f => Ord id => Graph id e n -> { nodes :: f (id /\ n), edges :: f (Pair id /\ e) }
toUnfoldables g = { nodes: G.nodesToUnfoldable g, edges: G.edgesToUnfoldable g }

--------------------------------------------------------------------------------
-- Node API
--------------------------------------------------------------------------------

memberNode :: forall id e n. Ord id => id -> Graph id e n -> Boolean
memberNode id g = isJust $ G.lookupNode id g

outgoingNodes :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Array (id /\ n))
outgoingNodes id graph = graph
  # G.outgoingIds id
  <#> S.toUnfoldable
  <#> map (\id' -> id' /\ (unsafePartial $ partLookupNode id' graph))

addNode :: forall id e n. Ord id => id -> n -> Graph id e n -> Maybe (Graph id e n)
addNode id _ g | isJust (G.lookupNode id g) = Nothing
addNode id n g = Just $ G.insertNode id n g

updateNode :: forall id e n. Ord id => id -> n -> Graph id e n -> Maybe (Graph id e n)
updateNode id node g = do
  _ <- G.lookupNode id g
  Just $ G.insertNode id node g

deleteNode :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Graph id e n)
deleteNode id g | not (memberNode id g) = Nothing
deleteNode id g = Just $ G.attemptDeleteNode id g

--------------------------------------------------------------------------------
-- Edge API
--------------------------------------------------------------------------------

memberEdge :: forall id e n. Ord id => Pair id -> Graph id e n -> Boolean
memberEdge conn g = isJust $ G.lookupEdge conn g

edgeIds :: forall id e n. Ord id => Graph id e n -> Set (Pair id)
edgeIds g = g
  # G.nodeIds
  # S.map (\from -> G.outgoingIds from g # fold # S.map (\to -> from ~ to))
  # S.unions

addEdge :: forall id e n. Ord id => Pair id -> e -> Graph id e n -> Maybe (Graph id e n)
addEdge conn _ g | memberEdge conn g = Nothing
addEdge conn e g = G.insertEdge conn e g

updateEdge :: forall id e n. Ord id => Pair id -> e -> Graph id e n -> Maybe (Graph id e n)
updateEdge conn e g = do
  _ <- G.lookupEdge conn g
  G.insertEdge conn e g

deleteEdge :: forall id e n. Ord id => Pair id -> Graph id e n -> Maybe (Graph id e n)
deleteEdge conn g | memberEdge conn g = Just $ G.attemptDeleteEdge conn g
deleteEdge _ _ = Nothing

--------------------------------------------------------------------------------

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
