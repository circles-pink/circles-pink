module Data.IxGraph.Errors where

import Prelude

import Data.Graph.Errors (ErrOutgoingIds)
import Type.Row (type (+))

type ErrOutgoingNodes id r = ErrOutgoingIds id + r 