module Data.Graph.Core
  ( Graph
  , deleteEdge
  , deleteNode
  , empty
  , incomingIds
  , insertEdge
  , insertNode
  , lookupEdge
  , lookupNode
  , nodeIds
  , outgoingIds
  ) where

import Prelude

import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as S

newtype Graph id e n = Graph
  ( Map id
      { data :: n
      , outEdges :: Map id e
      , inIds :: Set id
      }
  )

empty :: forall id e n. Graph id e n
empty = Graph M.empty

outgoingIds :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Set id)
outgoingIds id (Graph nodes) = M.lookup id nodes <#> _.outEdges >>> M.keys

incomingIds :: forall id e n. Ord id => id -> Graph id e n -> Maybe (Set id)
incomingIds id (Graph nodes) = M.lookup id nodes <#> _.inIds

nodeIds :: forall id e n. Graph id e n -> Set id
nodeIds (Graph nodes) = M.keys nodes

lookupNode :: forall id e n. Ord id => id -> Graph id e n -> Maybe n
lookupNode id (Graph nodes) = M.lookup id nodes <#> _.data

lookupEdge :: forall id e n. Ord id => id -> id -> Graph id e n -> Maybe e
lookupEdge from to (Graph nodes) = nodes
  # M.lookup from
  >>= _.outEdges >>> M.lookup to

insertNode :: forall id e n. Ord id => id -> n -> Graph id e n -> Graph id e n
insertNode id node (Graph nodes) = Graph $ M.alter f id nodes
  where
  f Nothing = Just { data: node, outEdges: M.empty, inIds: S.empty }
  f (Just x) = Just $ x { data = node }

insertEdge :: forall id e n. Ord id => id -> id -> e -> Graph id e n -> Graph id e n
insertEdge from to edge (Graph nodes) = Graph $ nodes
  # M.update updateFromNode from
  # M.update updateToNode to
  where
  updateFromNode x = Just $ x { outEdges = M.insert to edge x.outEdges }
  updateToNode x = Just $ x { inIds = S.insert from x.inIds }

deleteNode :: forall id e n. Ord id => id -> Graph id e n -> Graph id e n
deleteNode id graph@(Graph nodes) = Graph $ outIds
  # foldr (M.update updateToNode) nodes
  # M.delete id
  where
  outIds = maybe S.empty identity $ outgoingIds id graph
  updateToNode x = Just $ x { inIds = S.delete id x.inIds }

deleteEdge :: forall id e n. Ord id => id -> id -> Graph id e n -> Graph id e n
deleteEdge from to (Graph nodes) = Graph $ nodes
  # M.update updateFromNode from
  # M.update updateToNode to
  where
  updateFromNode x = Just $ x { outEdges = M.delete from x.outEdges }
  updateToNode x = Just $ x { inIds = S.delete to x.inIds }
