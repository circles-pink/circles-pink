module Data.IxGraph
  ( IxGraph
  , class Indexed
  , empty
  , getIndex
  , getNode
  , getOutgoinNodes
  , getOutgoingIds
  , insertEdge
  , insertEdges
  , insertNode
  , insertNodes
  , removeAtIds
  )
  where

import Prelude

import Control.Error.Util (note)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Tuple.Nested (type (/\))
import Debug.Extra (todo)

class Indexed k v | v -> k where
  getIndex :: v -> k

newtype IxGraph id e n = IxGraph
  { nodes :: Map id (NodeContext id n)
  , edges :: Map id (Map id e)
  }

type NodeContext id n =
  { data :: n
  , outgoing :: Set id
  , incoming :: Set id
  }

empty :: forall id e n. IxGraph id e n
empty = IxGraph { nodes: M.empty, edges: M.empty }

insertNode :: forall id e n. Indexed id n => n -> IxGraph id e n -> IxGraph id e n
insertNode = todo

getNode :: forall id e n.  id -> IxGraph id e n -> Maybe n
getNode = todo

getOutgoingIds :: forall id e n. id -> IxGraph id e n -> Set id
getOutgoingIds = todo

removeAtIds :: forall id e n. Set id -> IxGraph id e n -> IxGraph id e n
removeAtIds = todo

getOutgoinNodes :: forall id e n. id -> IxGraph id e n -> Set n
getOutgoinNodes = todo
insertEdge :: forall id e n. id -> id -> e -> IxGraph id e n -> IxGraph id e n
insertEdge = todo

insertEdges :: forall id e n. Array (id /\ id /\ e) -> IxGraph id e n -> IxGraph id e n
insertEdges = todo

insertNodes :: forall id e n. Array n -> IxGraph id e n -> IxGraph id e n
insertNodes = todo
