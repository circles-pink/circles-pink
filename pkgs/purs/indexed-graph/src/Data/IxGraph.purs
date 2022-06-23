module Data.IxGraph
  ( IxGraph
  , class Indexed
  , deleteNodes
  , empty
  , getIndex
  , insertEdge
  , insertEdges
  , insertNode
  , insertNodes
  , lookupNode
  , outgoingIds
  , outgoingNodes
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\))

class Indexed k v | v -> k where
  getIndex :: v -> k

newtype IxGraph id e n = IxGraph (Graph id e n)

instance showIxGraph :: (Show id, Show e, Show n) => Show (IxGraph id e n) where
  show (IxGraph g) = show g

empty :: forall id e n. IxGraph id e n
empty = IxGraph G.empty

insertNode :: forall id e n. Ord id => Indexed id n => n -> IxGraph id e n -> IxGraph id e n
insertNode node (IxGraph graph) = IxGraph $ G.insertNode (getIndex node) node graph

lookupNode :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe n
lookupNode id (IxGraph graph) = G.lookupNode id graph

outgoingIds :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe (Set id)
outgoingIds id (IxGraph graph) = G.outgoingIds id graph

deleteNodes :: forall f id e n. Ord id => Foldable f => f id -> IxGraph id e n -> IxGraph id e n
deleteNodes ids (IxGraph graph) = IxGraph $ G.deleteNodes ids graph

insertEdges :: forall f id e n. Ord id => Foldable f => f (id /\ id /\ e) -> IxGraph id e n -> IxGraph id e n
insertEdges edges (IxGraph graph) = IxGraph $ G.insertEdges edges graph

insertNodes :: forall f id e n. Foldable f => Ord id => Indexed id n => f n -> IxGraph id e n -> IxGraph id e n
insertNodes nodes ixGraph = foldr insertNode ixGraph nodes

insertEdge :: forall id e n. Ord id => id -> id -> e -> IxGraph id e n -> IxGraph id e n
insertEdge from to edge (IxGraph graph) = IxGraph $ G.insertEdge from to edge graph

outgoingNodes :: forall id e n. Ord id => Ord n => id -> IxGraph id e n -> Maybe (Set n)
outgoingNodes id (IxGraph graph) = G.outgoingNodes id graph