module Data.Graph.Errors where

import Data.Pair (Pair)
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

type ErrAll id r =
   ErrLookupNode id
    + ErrAddNode id
    + ErrUpdateNode id
    + ErrDeleteNode id
    + ErrLookupEdge id
    + ErrLookupEdge id
    + ErrAddEdge id
    + ErrUpdateEdge id
    + ErrDeleteEdge id
    + ErrOutgoingIds id
    + ErrIncomingIds id
    + ErrFromFoldables id
    + r

--------------------------------------------------------------------------------
-- Function Errors / Graph
--------------------------------------------------------------------------------

type ErrOutgoingNodes id r = ErrOutgoingIds id + r

type ErrOutgoingEdgesWithNodes id r = ErrOutgoingIds id + r

type ErrIncomingEdgesWithNodes id r = ErrIncomingIds id + r

type ErrNeighborIds id r = ErrOutgoingIds id + ErrIncomingIds id + r

type ErrNeighborEdgesWithNodes id r = ErrIncomingEdgesWithNodes id + ErrOutgoingEdgesWithNodes id + r
