module Data.IxGraph
  ( IxGraph
  ) where

import Prelude

import Data.Map (Map)
import Data.Set (Set)
import Data.Typelevel.Undefined (undefined)

class (Show k) <= Indexed k v | v -> k where
  getIndex :: v -> k

newtype IxGraph id e n = Graph
  { nodes :: Map id (NodeContext id n)
  , edges :: Map id (Map id e)
  }

type NodeContext id n =
  { data :: n
  , outgoing :: Set id
  , incoming :: Set id
  }

insertNode :: forall id e n. Indexed id n => n -> IxGraph id e n -> IxGraph id e n
insertNode = undefined

insertEdge :: forall id e n. id -> id -> e -> IxGraph id e n -> IxGraph id e n
insertEdge = undefined