module Data.Graph
  ( deleteNodes
  , fromFoldables
  , insertEdges
  , insertNodes
  , maybeInsertEdge
  , memberEdge
  , memberNode
  , module Exp
  )
  where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Graph.Core (Graph)
import Data.Graph.Core (Graph, deleteEdge, deleteNode, empty, incomingIds, insertEdge, insertNode, lookupEdge, lookupNode, outgoingIds) as Exp
import Data.Graph.Core as G
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested (type (/\), uncurry2, (/\))
import Debug.Extra (todo)

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
