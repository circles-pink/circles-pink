module Data.Graph
  ( NeighborConnectivity(..)
  , addNodes
  , edgeIds
  , edges
  , incomingEdges
  , incomingEdgesWithNodes
  , incomingNodes
  , insertEdge
  , insertNode
  , insertNodes
  , modifyNode
  , module Exp
  , neighborEdgesWithNodes
  , neighborIds
  , neighborNodes
  , neighborhood
  , nodes
  , outgoingEdges
  , outgoingEdgesWithNodes
  , outgoingNodes
  , unNeighborConnectivity
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight')
import Data.Foldable (class Foldable, fold, foldM)
import Data.Generic.Rep (class Generic)
import Data.Graph.Core (EitherV, Graph)
import Data.Graph.Core (GraphSpec, toUnfoldables, fromFoldables, EitherV, Graph, addEdge, addNode, deleteEdge, deleteNode, edgesToUnfoldable, empty, foldMapWithIndex, foldlWithIndex, foldrWithIndex, incomingIds, lookupEdge, lookupNode, memberEdge, memberNode, nodeIds, nodesToUnfoldable, outgoingIds, updateEdge, updateNode) as Exp
import Data.Graph.Core as C
import Data.Graph.Errors (ErrAddNodes, ErrIncomingEdges, ErrIncomingEdgesWithNodes, ErrIncomingNodes, ErrInsertEdge, ErrInsertNode, ErrInsertNodes, ErrModifyNode, ErrNeighborEdgesWithNodes, ErrNeighborIds, ErrNeighborNodes, ErrOutgoingEdges, ErrOutgoingEdgesWithNodes, ErrOutgoingNodes, ErrNeighborhood)
import Data.Pair (Pair, (~))
import Data.Set (Set)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

type IxNode id n = id /\ n
type IxEdge id e = Pair id /\ e

type IxEdgeWithNode id e n = IxEdge id e /\ IxNode id n

--------------------------------------------------------------------------------

data NeighborConnectivity e = JustOutgoing e | JustIncoming e | MutualOutAndIn e e

unNeighborConnectivity
  :: forall e z
   . { onJustOutgoing :: e -> z
     , onJustIncoming :: e -> z
     , onMutualOutAndIn :: e -> e -> z
     }
  -> NeighborConnectivity e
  -> z
unNeighborConnectivity { onJustOutgoing, onJustIncoming, onMutualOutAndIn } = case _ of
  JustOutgoing x1 -> onJustOutgoing x1
  JustIncoming x1 -> onJustIncoming x1
  MutualOutAndIn x1 x2 -> onMutualOutAndIn x1 x2

derive instance Generic (NeighborConnectivity e) _
derive instance Eq e => Eq (NeighborConnectivity e)
derive instance Ord e => Ord (NeighborConnectivity e)

instance showNeighborConnectivity :: Show e => Show (NeighborConnectivity e) where
  show = genericShow

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

incomingNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrIncomingNodes id r) (Array (id /\ n))
incomingNodes id graph = graph
  # C.incomingIds id
  <#> S.toUnfoldable
  <#> map (\id' -> id' /\ (unsafePartial $ partLookupNode id' graph))

neighborNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrNeighborNodes id r) (Array (IxNode id n))
neighborNodes id g = (<>) <$> incomingNodes id g <*> outgoingNodes id g

addNodes :: forall r f id e n. Foldable f => Ord id => f (id /\ n) -> Graph id e n -> EitherV (ErrAddNodes id r) (Graph id e n)
addNodes nodes' g = foldM (flip $ uncurry C.addNode) g nodes'

insertNode :: forall r id e n. Ord id => id -> n -> Graph id e n -> EitherV (ErrInsertNode r) (Graph id e n)
insertNode id node graph | C.memberNode id graph = C.updateNode id node graph # lmap (\_ -> unsafePartial $ crashWith "impossible")
insertNode id node graph = C.addNode id node graph # lmap (\_ -> unsafePartial $ crashWith "impossible")

insertNodes :: forall r f id e n. Foldable f => Ord id => f (id /\ n) -> Graph id e n -> EitherV (ErrInsertNodes r) (Graph id e n)
insertNodes nodes' g = foldM (flip $ uncurry insertNode) g nodes'

modifyNode :: forall r id e n. Ord id => id -> (n -> n) -> Graph id e n -> EitherV (ErrModifyNode id r) (Graph id e n)
modifyNode id f g = do
  n <- C.lookupNode id g
  C.updateNode id (f n) g

--------------------------------------------------------------------------------
-- Edge API
--------------------------------------------------------------------------------

edgeIds :: forall id e n. Ord id => Graph id e n -> Set (Pair id)
edgeIds g = g
  # C.nodeIds
  # S.map (\from -> C.outgoingIds from g # fold # S.map (\to -> from ~ to))
  # S.unions

insertEdge :: forall r id e n. Ord id => Pair id -> e -> Graph id e n -> EitherV (ErrInsertEdge r) (Graph id e n)
insertEdge id node graph | C.memberEdge id graph = C.updateEdge id node graph # lmap (\_ -> unsafePartial $ crashWith "impossible")
insertEdge id node graph = C.addEdge id node graph # lmap (\_ -> unsafePartial $ crashWith "impossible")

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

outgoingEdges :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrOutgoingEdges id r) (Array (IxEdge id e))
outgoingEdges fromId graph = do
  ids <- C.outgoingIds fromId graph
  ids # S.toUnfoldable <#> getEdge # pure
  where
  getEdge toId = (unsafePartial $ partLookupEdgeIx (fromId ~ toId) graph)

incomingEdgesWithNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrIncomingEdgesWithNodes id r) (Array (IxEdgeWithNode id e n))
incomingEdgesWithNodes fromId graph = do
  ids <- C.incomingIds fromId graph
  ids # S.toUnfoldable <#> getBoth # pure
  where
  getBoth toId = (unsafePartial $ partLookupEdgeIx (toId ~ fromId) graph) /\
    (unsafePartial $ partLookupNodeIx toId graph)

incomingEdges :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrIncomingEdges id r) (Array (IxEdge id e))
incomingEdges fromId graph = do
  ids <- C.incomingIds fromId graph
  ids # S.toUnfoldable <#> getEdge # pure
  where
  getEdge toId = (unsafePartial $ partLookupEdgeIx (toId ~ fromId) graph)

neighborEdgesWithNodes :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrNeighborEdgesWithNodes id r) (Array (IxEdgeWithNode id e n))
neighborEdgesWithNodes id g = (<>) <$> incomingEdgesWithNodes id g <*> outgoingEdgesWithNodes id g

neighborhood :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrNeighborhood id r) (Array (NeighborConnectivity e /\ id /\ n))
neighborhood id g = g # neighborIds id <#> S.toUnfoldable <#> map (\id' -> getEdges id' /\ getIxNode id')
  where
  getEdges id' = case C.lookupEdge (id ~ id') g, C.lookupEdge (id' ~ id) g of
    Right outgoing, Right incoming -> MutualOutAndIn outgoing incoming
    Right outgoing, Left _ -> JustOutgoing outgoing
    Left _, Right incoming -> JustIncoming incoming
    Left _, Left _ -> unsafeCrashWith "inpossible case"
  getIxNode id' = id' /\ unsafePartial (partLookupNode id' g)

--------------------------------------------------------------------------------

partLookupNode :: forall id e n. Partial => Ord id => id -> Graph id e n -> n
partLookupNode id' g = fromRight' (\_ -> crashWith "Inconsistency: Node not found") $ C.lookupNode id' g

partLookupNodeIx :: forall id e n. Partial => Ord id => id -> Graph id e n -> IxNode id n
partLookupNodeIx id' g = id' /\ partLookupNode id' g

partLookupEdge :: forall id e n. Partial => Ord id => Pair id -> Graph id e n -> e
partLookupEdge conn g = fromRight' (\_ -> crashWith "Inconsistency: Edge not found") $ C.lookupEdge conn g

partLookupEdgeIx :: forall id e n. Partial => Ord id => Pair id -> Graph id e n -> IxEdge id e
partLookupEdgeIx conn g = conn /\ partLookupEdge conn g
