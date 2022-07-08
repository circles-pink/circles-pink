module Data.IxGraph
  ( IxGraph
  , addEdge
  , addNode
  , addNodes
  , class Indexed
  , deleteEdge
  , deleteNode
  , empty
  , getIndex
  , incomingEdgesWithNodes
  , lookupEdge
  , lookupNode
  , neighborEdgesWithNodes
  , outgoingEdgesWithNodes
  , outgoingIds
  , outgoingNodes
  , toUnfoldables
  , updateEdge
  , updateNode
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Graph (EitherV, Graph, GraphSpec)
import Data.Graph as G
import Data.Graph.Errors (ErrAddNode, ErrAddNodes, ErrDeleteEdge, ErrDeleteNode, ErrIncomingEdgesWithNodes, ErrLookupEdge, ErrLookupNode, ErrNeighborEdgesWithNodes, ErrOutgoingEdgesWithNodes, ErrOutgoingIds, ErrOutgoingNodes, ErrUpdateEdge, ErrUpdateNode, ErrAddEdge)
import Data.Pair (Pair)
import Data.Set (Set)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)

class Indexed k v | v -> k where
  getIndex :: v -> k

instance indexedEither :: (Indexed k a, Indexed k b) => Indexed k (Either a b) where
  getIndex (Left x) = getIndex x
  getIndex (Right x) = getIndex x

newtype IxGraph id e n = IxGraph (Graph id e n)

--------------------------------------------------------------------------------
-- Graph API
--------------------------------------------------------------------------------

empty :: forall id e n. IxGraph id e n
empty = IxGraph G.empty

--------------------------------------------------------------------------------
-- Node API
--------------------------------------------------------------------------------

addNode :: forall r id e n. Ord id => Indexed id n => n -> IxGraph id e n -> EitherV (ErrAddNode id r) (IxGraph id e n)
addNode n (IxGraph g) = IxGraph <$> G.addNode (getIndex n) n g

lookupNode :: forall r id e n. Ord id => id -> IxGraph id e n -> EitherV (ErrLookupNode id r) n
lookupNode id (IxGraph graph) = G.lookupNode id graph

updateNode :: forall r id e n. Ord id => Indexed id n => n -> IxGraph id e n -> EitherV (ErrUpdateNode id r) (IxGraph id e n)
updateNode n (IxGraph g) = IxGraph <$> G.updateNode (getIndex n) n g

deleteNode :: forall r id e n. Ord id => id -> IxGraph id e n -> EitherV (ErrDeleteNode id r) (IxGraph id e n)
deleteNode id (IxGraph g) = IxGraph <$> G.deleteNode id g

addNodes :: forall r f id e n. Foldable f => Functor f => Indexed id n => Ord id => f n -> IxGraph id e n -> EitherV (ErrAddNodes id r) (IxGraph id e n)
addNodes nodes (IxGraph g) = IxGraph <$> G.addNodes (withIndex <$> nodes) g


--------------------------------------------------------------------------------
-- Edge API
--------------------------------------------------------------------------------

addEdge :: forall r id e n. Ord id => Indexed (Pair id) e => e -> IxGraph id e n -> EitherV (ErrAddEdge id r) (IxGraph id e n)
addEdge e (IxGraph g) = IxGraph <$> G.addEdge (getIndex e) e g

lookupEdge :: forall r id e n. Ord id => Pair id -> IxGraph id e n -> EitherV (ErrLookupEdge id r) e
lookupEdge conn (IxGraph graph) = G.lookupEdge conn graph

updateEdge :: forall r id e n. Ord id => Indexed (Pair id) e => e -> IxGraph id e n -> EitherV (ErrUpdateEdge id r) (IxGraph id e n)
updateEdge e (IxGraph g) = IxGraph <$> G.updateEdge (getIndex e) e g

deleteEdge :: forall r id e n. Ord id => Pair id -> IxGraph id e n -> EitherV (ErrDeleteEdge id r) (IxGraph id e n)
deleteEdge conn (IxGraph g) = IxGraph <$> G.deleteEdge conn g

--------------------------------------------------------------------------------

outgoingIds :: forall r id e n. Ord id => id -> IxGraph id e n -> EitherV (ErrOutgoingIds id r) (Set id)
outgoingIds id (IxGraph graph) = G.outgoingIds id graph

outgoingNodes :: forall r id e n. Ord id => id -> IxGraph id e n -> EitherV (ErrOutgoingNodes id r) (Array n)
outgoingNodes id (IxGraph graph) = map snd <$> G.outgoingNodes id graph

outgoingEdgesWithNodes :: forall r id e n. Ord id => Ord e => Ord n => id -> IxGraph id e n -> EitherV (ErrOutgoingEdgesWithNodes id r) (Array (e /\ n))
outgoingEdgesWithNodes id (IxGraph graph) = map (bimap snd snd) <$> G.outgoingEdgesWithNodes id graph

incomingEdgesWithNodes :: forall r id e n. Ord id => Ord e => Ord n => id -> IxGraph id e n -> EitherV (ErrIncomingEdgesWithNodes id r) (Array (e /\ n))
incomingEdgesWithNodes id (IxGraph graph) = map (bimap snd snd) <$> G.incomingEdgesWithNodes id graph

neighborEdgesWithNodes :: forall r id e n. Ord id => Ord e => Ord n => id -> IxGraph id e n -> EitherV (ErrNeighborEdgesWithNodes id r) (Array (e /\ n))
neighborEdgesWithNodes id (IxGraph graph) = map (bimap snd snd) <$> G.neighborEdgesWithNodes id graph

toUnfoldables :: forall id e n f. Unfoldable f => Ord id => IxGraph id e n -> GraphSpec f id e n
toUnfoldables (IxGraph graph) = G.toUnfoldables graph

-- insertNode :: forall id e n. Ord id => Indexed id n => n -> IxGraph id e n -> IxGraph id e n
-- insertNode node (IxGraph graph) = IxGraph $ G.insertNode (getIndex node) node graph

-- deleteNodes :: forall f id e n. Ord id => Foldable f => f id -> IxGraph id e n -> IxGraph id e n
-- deleteNodes ids (IxGraph graph) = IxGraph $ G.deleteNodes ids graph

-- deleteEdges :: forall f id e n. Ord id => Foldable f => f (Pair id) -> IxGraph id e n -> IxGraph id e n
-- deleteEdges ids (IxGraph graph) = IxGraph $ G.deleteEdges ids graph

-- deleteEdge :: forall id e n. Ord id => Pair id -> IxGraph id e n -> IxGraph id e n
-- deleteEdge conn (IxGraph graph) = IxGraph $ G.deleteEdge conn graph

-- insertEdges :: forall f id e n. Ord id => Functor f => Foldable f => Indexed (Pair id) e => f e -> IxGraph id e n -> IxGraph id e n
-- insertEdges edges (IxGraph graph) = IxGraph $ G.insertEdges (withIndex <$> edges) graph

-- insertNodes :: forall f id e n. Foldable f => Ord id => Indexed id n => f n -> IxGraph id e n -> IxGraph id e n
-- insertNodes nodes ixGraph = foldr insertNode ixGraph nodes

-- insertEdge :: forall id e n. Ord id => Indexed (Pair id) e => e -> IxGraph id e n -> IxGraph id e n
-- insertEdge edge (IxGraph graph) = IxGraph $ G.insertEdge (getIndex edge) edge graph

--------------------------------------------------------------------------------

withIndex :: forall i a. Indexed i a => a -> i /\ a
withIndex x = getIndex x /\ x
