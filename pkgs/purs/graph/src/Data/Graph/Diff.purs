module Data.Graph.Diff
  ( DiffInstruction(..)
  , applyDiff
  , getDiff
  , mustAlwaysBeTrue
  ) where

import Prelude

import Data.Graph (Graph)
import Data.Pair (Pair)
import Debug.Extra (todo)

data DiffInstruction id e n
  = AddNode n
  | DeleteNode id
  | UpdateNode n
  | AddEdge e
  | DeleteEdge (Pair id)
  | UpdateEdge e

type GraphDiff id e n = Array (DiffInstruction id e n)

getDiff :: forall id e n. Graph id e n -> Graph id e n -> GraphDiff id e n
getDiff = todo

applyDiff :: forall id e n. GraphDiff id e n -> Graph id e n -> Graph id e n
applyDiff = todo

mustAlwaysBeTrue
  :: forall id e n
   . Eq id
  => Eq e
  => Eq n
  => Graph id e n
  -> Graph id e n
  -> Boolean
mustAlwaysBeTrue g1 g2 =
  let
    diff = getDiff g1 g2
  in
    applyDiff diff g1 == g2

