module Data.Graph.Errors where

import Prelude

import Data.Pair (Pair, (~))
import Data.Variant (Variant)
import Data.Variant (case_, onMatch)
import Debug.Extra (todo)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- Row Slices
--------------------------------------------------------------------------------

type NodeNotFound (id :: Type) r = (nodeNotFound :: id | r)

type NodeExists (id :: Type) r = (nodeExists :: id | r)

type StartNodeNotFound (id :: Type) r = (startNodeNotFound :: id | r)

type EndNodeNotFound (id :: Type) r = (endNodeNotFound :: id | r)

type EdgeNotFound id r = (edgeNotFound :: Pair id | r)

type EdgeExists id r = (edgeExists :: Pair id | r)

type ErrAll id r =
  NodeNotFound id
    + NodeExists id
    + StartNodeNotFound id
    + EndNodeNotFound id
    + EdgeNotFound id
    + EdgeExists id
    + r

--------------------------------------------------------------------------------
-- Proxies
--------------------------------------------------------------------------------

_nodeNotFound :: Proxy "nodeNotFound"
_nodeNotFound = Proxy

_nodeExists :: Proxy "nodeExists"
_nodeExists = Proxy

_startNodeNotFound :: Proxy "startNodeNotFound"
_startNodeNotFound = Proxy

_endNodeNotFound :: Proxy "endNodeNotFound"
_endNodeNotFound = Proxy

_edgeNotFound :: Proxy "edgeNotFound"
_edgeNotFound = Proxy

_edgeExists :: Proxy "edgeExists"
_edgeExists = Proxy

--------------------------------------------------------------------------------
-- Function Errors / Core
--------------------------------------------------------------------------------
type ErrLookupNode id r = NodeNotFound id + r

type ErrAddNode id r = NodeExists id + r

type ErrUpdateNode id r = NodeNotFound id + r

type ErrDeleteNode id r = NodeNotFound id + r

type ErrLookupEdge id r = StartNodeNotFound id + EndNodeNotFound id + EdgeNotFound id + r

type ErrAddEdge id r = StartNodeNotFound id + EndNodeNotFound id + EdgeExists id + r

type ErrUpdateEdge id r = StartNodeNotFound id + EndNodeNotFound id + EdgeNotFound id + r

type ErrDeleteEdge id r = StartNodeNotFound id + EndNodeNotFound id + EdgeNotFound id + r

type ErrOutgoingIds id r = NodeNotFound id + r

type ErrIncomingIds id r = NodeNotFound id + r

type ErrFromFoldables id r = ErrAddNode id + ErrAddEdge id + r

type ErrAddNodes id r = ErrAddNode id + r

--------------------------------------------------------------------------------
-- Function Errors / Graph
--------------------------------------------------------------------------------

type ErrOutgoingNodes id r = ErrOutgoingIds id + r

type ErrOutgoingEdgesWithNodes id r = ErrOutgoingIds id + r

type ErrIncomingEdgesWithNodes id r = ErrIncomingIds id + r

type ErrNeighborIds id r = ErrOutgoingIds id + ErrIncomingIds id + r

type ErrNeighborEdgesWithNodes id r = ErrIncomingEdgesWithNodes id + ErrOutgoingEdgesWithNodes id + r

type ErrInsertNode :: forall k. k -> k
type ErrInsertNode r = r

type ErrInsertNodes :: forall k. k -> k
type ErrInsertNodes r = r


--------------------------------------------------------------------------------
-- Pretty Print
--------------------------------------------------------------------------------

printError :: forall id. Show id => Variant (ErrAll id ()) -> String
printError = case_
  # onMatch
      { nodeNotFound: \id -> "Node with id `" <> show id <> "` not found."
      , nodeExists: \id -> "Node with id `" <> show id <> "` already exists."
      , startNodeNotFound: \id -> "Start node with id `" <> show id <> "` not found."
      , endNodeNotFound: \id -> "End node with id `" <> show id <> "` not found."
      , edgeNotFound: \(from ~ to) -> "Edge from node id `" <> show from <> "` to node id `" <> show to <> " ` not found."
      , edgeExists: \(from ~ to) -> "Edge from node id `" <> show from <> "` to node id `" <> show to <> " ` already exists."
      }
