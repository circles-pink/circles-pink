module Data.Graph.Diff
  ( DiffInstruction(..)
  , GraphDiff
  , applyDiff
  , getDiff
  , getEdgesDiff
  , getNodesDiff
  , isStructuralChange
  , spec
  , unDiffInstruction
  )
  where

import Prelude

import Data.Array (catMaybes, foldl, sort)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Graph (Graph, nodeIds)
import Data.Graph as G
import Data.Maybe (Maybe(..))
import Data.Pair (Pair)
import Data.Set (difference, intersection, toUnfoldable)
import Data.Show.Generic (genericShow)
import Debug.Extra (todo)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (withHelp)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

data DiffInstruction id e n
  = AddNode id n
  | AddEdge (Pair id) e
  | UpdateNode id n
  | UpdateEdge (Pair id) e
  | DeleteEdge (Pair id)
  | DeleteNode id

unDiffInstruction
  :: { onAddEdge :: _
     , onDeleteEdge :: _
     , onUpdateEdge :: _
     , onAddNode :: _
     , onDeleteNode :: _
     , onUpdateNode :: _
     }
  -> _
unDiffInstruction on di = case di of
  AddEdge x1 x2 -> on.onAddEdge x1 x2
  DeleteEdge x1 -> on.onDeleteEdge x1
  UpdateEdge x1 x2 -> on.onUpdateEdge x1 x2
  AddNode x1 x2 -> on.onAddNode x1 x2
  DeleteNode x1 -> on.onDeleteNode x1
  UpdateNode x1 x2 -> on.onUpdateNode x1 x2

type GraphDiff id e n = Array (DiffInstruction id e n)

getDiff
  :: forall id e n
   . Ord id
  => Ord e
  => Ord n
  => Graph id e n
  -> Graph id e n
  -> GraphDiff id e n
getDiff g1 g2 = sort (getNodesDiff g1 g2 <> getEdgesDiff g1 g2)

getNodesDiff :: forall id e n. Ord id => Eq n => Graph id e n -> Graph id e n -> GraphDiff id e n
getNodesDiff g1 g2 =
  let
    g1ids = G.nodeIds g1
    g2ids = G.nodeIds g2
    inBoth = toUnfoldable $ intersection g1ids g2ids
    g1Only = toUnfoldable $ difference g1ids g2ids
    g2Only = toUnfoldable $ difference g2ids g1ids
    deleteNodes = map DeleteNode g1Only
    addNodes = map (\id -> AddNode id (unsafePartial G.partLookupNode id g2)) g2Only
    updateNodes = inBoth
      # map
          ( \id ->
              let
                n1 = unsafePartial G.partLookupNode id g1
                n2 = unsafePartial G.partLookupNode id g2
              in
                if n1 == n2 then Nothing else Just $ UpdateNode id n2
          )
      # catMaybes
  in
    deleteNodes <> addNodes <> updateNodes

getEdgesDiff :: forall id e n. Ord id => Eq e => Graph id e n -> Graph id e n -> GraphDiff id e n
getEdgesDiff g1 g2 =
  let
    g1ids = G.edgeIds g1
    g2ids = G.edgeIds g2
    inBoth = toUnfoldable $ intersection g1ids g2ids
    g1Only = toUnfoldable $ difference g1ids g2ids
    g2Only = toUnfoldable $ difference g2ids g1ids
    deleteEdges = map DeleteEdge g1Only
    addEdges = map (\id -> AddEdge id (unsafePartial G.partLookupEdge id g2)) g2Only
    updateEdges = inBoth
      # map
          ( \id ->
              let
                e1 = unsafePartial G.partLookupEdge id g1
                e2 = unsafePartial G.partLookupEdge id g2
              in
                if e1 == e2 then Nothing else Just $ UpdateEdge id e2
          )
      # catMaybes
  in
    deleteEdges <> addEdges <> updateEdges

applyDiff :: forall id e n. Ord id => GraphDiff id e n -> Graph id e n -> Graph id e n
applyDiff di g = foldl applyDiffInstruction g di

applyDiffInstruction :: forall id e n. Ord id => Graph id e n -> DiffInstruction id e n -> Graph id e n
applyDiffInstruction g di = either (const g) identity
  case di of
    DeleteEdge id -> G.deleteEdge id g
    AddEdge id n -> G.addEdge id n g
    UpdateEdge id n -> G.updateEdge id n g
    DeleteNode id -> G.deleteNode id g
    AddNode id n -> G.addNode id n g
    UpdateNode id n -> G.updateNode id n g

spec :: Spec Unit
spec = describe "Graph diff" do
  it "getDiff and applyDiff are correct" do
    quickCheck \(g1 :: _ Char String Boolean) g2 ->
      let
        diff = getDiff g1 g2
        g2_ = applyDiff diff g1
      in
        (g2_ == g2) `withHelp` (show { diff, g1, g2, g2_ } <> "\n")

derive instance Generic (DiffInstruction id e n) _

derive instance (Eq id, Eq e, Eq n) => Eq (DiffInstruction id e n)

derive instance (Ord id, Ord e, Ord n) => Ord (DiffInstruction id e n)

instance (Show id, Show e, Show n) => Show (DiffInstruction id e n) where
  show = genericShow

isStructuralChange :: forall id e n. Eq id => Ord id => Graph id e n -> Graph id e n -> Boolean
isStructuralChange g1 g2 = G.nodeIds g1 == G.nodeIds g2 && G.edgeIds g1 == G.edgeIds g2
