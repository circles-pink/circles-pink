module Data.IxGraph
  ( IxGraph
  , class Indexed
  , deleteEdges
  , deleteNodes
  , empty
  , getIndex
  , insertEdge
  , insertEdges
  , insertNode
  , insertNodes
  , lookupEdge
  , lookupNode
  , outgoingEdgesWithNodes
  , outgoingIds
  , outgoingNodes
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as S
import Data.Tuple.Nested (type (/\))

class Indexed k v | v -> k where
  getIndex :: v -> k

instance indexedEither :: (Indexed k a, Indexed k b) => Indexed k (Either a b) where
  getIndex (Left x) = getIndex x
  getIndex (Right x) = getIndex x

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

deleteEdges :: forall f id e n. Ord id => Foldable f => f (id /\ id) -> IxGraph id e n -> IxGraph id e n
deleteEdges ids (IxGraph graph) = IxGraph $ G.deleteEdges ids graph

insertEdges :: forall f id e n. Ord id => Foldable f => f (id /\ id /\ e) -> IxGraph id e n -> IxGraph id e n
insertEdges edges (IxGraph graph) = IxGraph $ G.insertEdges edges graph

insertNodes :: forall f id e n. Foldable f => Ord id => Indexed id n => f n -> IxGraph id e n -> IxGraph id e n
insertNodes nodes ixGraph = foldr insertNode ixGraph nodes

insertEdge :: forall id e n. Ord id => id -> id -> e -> IxGraph id e n -> IxGraph id e n
insertEdge from to edge (IxGraph graph) = IxGraph $ G.insertEdge from to edge graph

outgoingNodes :: forall id e n. Ord id => Ord n => id -> IxGraph id e n -> Maybe (Set n)
outgoingNodes id (IxGraph graph) = S.fromFoldable <$> G.outgoingNodes id graph

lookupEdge :: forall id e n. Ord id => id -> id -> IxGraph id e n -> Maybe e
lookupEdge from to (IxGraph graph) = G.lookupEdge from to graph

outgoingEdgesWithNodes :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe (Array (e /\ n))
outgoingEdgesWithNodes id (IxGraph graph) = G.outgoingEdgesWithNodes id graph