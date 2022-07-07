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
import Data.Foldable (class Foldable, foldM)
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe)
import Data.Pair (Pair)
import Data.Set (Set)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
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

addNode :: forall id e n. Ord id => Indexed id n => n -> IxGraph id e n -> Maybe (IxGraph id e n)
addNode n (IxGraph g) = IxGraph <$> G.addNode (getIndex n) n g

lookupNode :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe n
lookupNode id (IxGraph graph) = G.lookupNode id graph

updateNode :: forall id e n. Ord id => Indexed id n => n -> IxGraph id e n -> Maybe (IxGraph id e n)
updateNode n (IxGraph g) = IxGraph <$> G.updateNode (getIndex n) n g

deleteNode :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe (IxGraph id e n)
deleteNode id (IxGraph g) = IxGraph <$> G.deleteNode id g

--

addNodes :: forall f id e n. Foldable f => Ord id => Indexed id n => f n -> IxGraph id e n -> IxGraph id e n
addNodes nodes ixGraph = foldM addNode ixGraph nodes

--------------------------------------------------------------------------------
-- Edge API
--------------------------------------------------------------------------------

addEdge :: forall id e n. Ord id => Indexed (Pair id) e => e -> IxGraph id e n -> Maybe (IxGraph id e n)
addEdge e (IxGraph g) = IxGraph <$> G.addEdge (getIndex e) e g

lookupEdge :: forall id e n. Ord id => Pair id -> IxGraph id e n -> Maybe e
lookupEdge conn (IxGraph graph) = G.lookupEdge conn graph

updateEdge :: forall id e n. Ord id => Indexed (Pair id) e => e -> IxGraph id e n -> Maybe (IxGraph id e n)
updateEdge e (IxGraph g) = IxGraph <$> G.updateEdge (getIndex e) e g

deleteEdge :: forall id e n. Ord id => Pair id -> IxGraph id e n -> Maybe (IxGraph id e n)
deleteEdge conn (IxGraph g) = IxGraph <$> G.deleteEdge conn g

--------------------------------------------------------------------------------

outgoingIds :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe (Set id)
outgoingIds id (IxGraph graph) = G.outgoingIds id graph

outgoingNodes :: forall id e n. Ord id => id -> IxGraph id e n -> Maybe (Array n)
outgoingNodes id (IxGraph graph) = map snd <$> G.outgoingNodes id graph

outgoingEdgesWithNodes :: forall id e n. Ord id => Ord e => Ord n => id -> IxGraph id e n -> Maybe (Array (e /\ n))
outgoingEdgesWithNodes id (IxGraph graph) = map (bimap snd snd) <$> G.outgoingEdgesWithNodes id graph

incomingEdgesWithNodes :: forall id e n. Ord id => Ord e => Ord n => id -> IxGraph id e n -> Maybe (Array (e /\ n))
incomingEdgesWithNodes id (IxGraph graph) = map (bimap snd snd) <$> G.incomingEdgesWithNodes id graph

neighborEdgesWithNodes :: forall id e n. Ord id => Ord e => Ord n => id -> IxGraph id e n -> Maybe (Array (e /\ n))
neighborEdgesWithNodes id (IxGraph graph) = map (bimap snd snd) <$> G.neighborEdgesWithNodes id graph

toUnfoldables :: forall id e n f. Unfoldable f => Ord id => IxGraph id e n -> { nodes :: f (id /\ n), edges :: f (Pair id /\ e) }
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

-- withIndex :: forall i a. Indexed i a => a -> i /\ a
-- withIndex x = getIndex x /\ x
