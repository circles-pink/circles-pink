module Data.Graph.Diff
  ( DiffInstruction(..)
  , applyDiff
  , getDiff
  , mustAlwaysBeTrue
  , spec
  ) where

import Prelude

import Data.Graph (Graph)
import Data.Pair (Pair)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Debug.Extra (todo)
import Test.QuickCheck (mkSeed, (/==), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck, quickCheck', quickCheckPure)

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

spec :: Spec Unit
spec = describe "Graph diff" do
  it "bla" do
    quickCheckPure (mkSeed 2785) 5 \(x /\ y) -> 
      let 
        _ = spy "x" x
        _ = spy "y" y
      in
      x + 2 /== y + 2 
    quickCheckPure (mkSeed 2785) 100 \(xs :: Graph Char String Boolean) -> 
      let 
        _ = spy "xs" xs
      in
      xs === xs
  
  it "getDiff and applyDiff are correct" do
    quickCheck \(g1 :: _ Char String Boolean) g2 -> 
      let
        diff = getDiff g1 g2
      in
        applyDiff diff g1 == g2

  describe "Math" do
    it "works" $
      quickCheck (\n -> (n * 2 / 2) === n)
    it "works again" $
      quickCheck \n -> ((n + 1) * 2) /== n