module Data.Graph.Diff
  ( DiffInstruction(..)
  , applyDiff
  , getDiff
  , spec
  ) where

import Prelude

import Data.Array (catMaybes, foldr)
import Data.Either (either)
import Data.Graph (Graph)
import Data.Graph as G
import Data.Maybe (Maybe(..))
import Data.Pair (Pair)
import Data.Set (difference, intersection, toUnfoldable)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

data DiffInstruction id e n
  = AddNode id n
  | DeleteNode id
  | UpdateNode id n
  | AddEdge (Pair id) e
  | DeleteEdge (Pair id)
  | UpdateEdge (Pair id) e

type GraphDiff id e n = Array (DiffInstruction id e n)

getDiff :: forall id e n. Ord id => Eq n => Graph id e n -> Graph id e n -> GraphDiff id e n
getDiff g1 g2 =
  let
    g1ids = G.nodeIds g1
    g2ids = G.nodeIds g2
    inBoth = toUnfoldable $ intersection g1ids g2ids
    g1Only = toUnfoldable $ difference g1ids g2ids
    g2Only = toUnfoldable $ difference g2ids g1ids
    deleteNodes = map (\id -> DeleteNode id) g1Only
    addNodes = map (\id -> AddNode id (unsafePartial G.partLookupNode id g2)) g2Only
    updateNodes = catMaybes $ map
      ( \id ->
          let
            n1 = unsafePartial G.partLookupNode id g1
            n2 = unsafePartial G.partLookupNode id g2
          in
            if n1 == n2 then Nothing else Just $ UpdateNode id n2
      )
      inBoth
  in
    deleteNodes <> addNodes <> updateNodes

applyDiff :: forall id e n. Ord id => GraphDiff id e n -> Graph id e n -> Graph id e n
applyDiff di g = foldr applyDiffInstruction g di
  where
  applyDiffInstruction :: DiffInstruction id e n -> Graph id e n -> Graph id e n
  applyDiffInstruction di' g' = case di' of
    DeleteNode id -> G.deleteNode id g # either (const g) identity
    AddNode id n -> G.addNode id n g' # either (const g) identity
    UpdateNode id n -> G.updateNode id n g' # either (const g) identity
    _ -> g'

spec :: Spec Unit
spec = describe "Graph diff" do
  -- it "bla" do
  -- quickCheckPure (mkSeed 2785) 5 \(x /\ y) ->
  --   let
  --     _ = spy "x" x
  --     _ = spy "y" y
  --   in
  --     x + 2 /== y + 2
  -- quickCheckPure (mkSeed 2785) 100 \(xs :: Graph Char String Boolean) ->
  --   let
  --     _ = spy "xs" xs
  --   in
  --     xs === xs

  it "getDiff and applyDiff are correct" do
    quickCheck \(g1 :: _ Char String Boolean) g2 ->
      let
        diff = getDiff g1 g2
      in
        applyDiff diff g1 === g2

-- describe "Math" do
--   it "works" $
--     quickCheck (\n -> (n * 2 / 2) === n)
--   it "works again" $
--     quickCheck \n -> ((n + 1) * 2) /== n
