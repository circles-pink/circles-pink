module Data.IxGraph.Diff where

import Prelude

import CirclesPink.GenerateTSD.Wrappers (IxGraph)
import Debug.Extra (todo)

data DiffInstruction = 
   AddNode | DeleteNode | AddEdge 

type GraphDiff = Array DiffInstruction

getDiff :: forall id e n. IxGraph id e n -> IxGraph id e n -> GraphDiff
getDiff = todo

applyDiff :: forall id e n. GraphDiff -> IxGraph id e n -> IxGraph id e n
applyDiff = todo

mustAlwaysBeTrue :: forall id e n. IxGraph id e n -> IxGraph id e n -> Boolean
mustAlwaysBeTrue g1 g2 =
  let
    diff = getDiff g1 g2
  in
    applyDiff diff g1 == g2

