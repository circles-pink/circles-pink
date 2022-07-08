module Test.Data.Graph.Core
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Graph.Core (EitherV, Graph, GraphSpec)
import Data.Graph.Core as C
import Data.Graph.Errors (ErrAll, _edgeExists, _edgeNotFound, _endNodeNotFound, _nodeExists, _nodeNotFound, _startNodeNotFound)
import Data.Pair ((~))
import Data.Tuple.Nested ((/\))
import Data.Variant (inj)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type TestGraph = Graph GraphId GraphEdge GraphNode

type GraphId = String

type GraphEdge = Int

type GraphNode = Int

spec :: Spec Unit
spec =
  let
    -- | Monomorphic `fromFoldables` for better type inference in tests
    fromFoldables :: GraphSpec Array GraphId GraphEdge GraphNode -> EitherV (ErrAll GraphId ()) TestGraph
    fromFoldables = C.fromFoldables
  in
    describe "Data.Graph.Core" do
      describe "addNode" do
        describe "node already exists" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.addNode "n1" 1) `shouldEqual` (Left $ inj _nodeExists "n1")
        describe "node does not exists" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns a new graph" do
            (graph >>= C.addNode "n1" 1) `shouldEqual`
              fromFoldables
                { nodes: [ "n1" /\ 1 ]
                , edges: []
                }
      describe "lookupNode" do
        describe "node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.lookupNode "n1") `shouldEqual` (Left $ inj _nodeNotFound "n1")
        describe "node found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns the node" do
            (graph >>= C.lookupNode "n1") `shouldEqual` (Right 1)
      describe "updateNode" do
        describe "node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.updateNode "n1" 2) `shouldEqual` (Left $ inj _nodeNotFound "n1")
        describe "node found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns a new graph" do
            (graph >>= C.updateNode "n1" 2) `shouldEqual` fromFoldables
              { nodes: [ "n1" /\ 2 ]
              , edges: []
              }
      describe "deleteNode" do
        describe "node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.deleteNode "n1") `shouldEqual` (Left $ inj _nodeNotFound "n1")
        describe "node found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns a new graph" do
            (graph >>= C.deleteNode "n1") `shouldEqual` fromFoldables
              { nodes: []
              , edges: []
              }
      describe "addEdge" do
        describe "start node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.addEdge ("n1" ~ "n2") 1) `shouldEqual` (Left $ inj _startNodeNotFound "n1")
        describe "end node not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.addEdge ("n1" ~ "n2") 1) `shouldEqual` (Left $ inj _endNodeNotFound "n2")
        describe "edge already exists" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: [ ("n1" ~ "n2") /\ 3 ]
              }
          it "returns an error" do
            (graph >>= C.addEdge ("n1" ~ "n2") 1) `shouldEqual` (Left $ inj _edgeExists ("n1" ~ "n2"))
        describe "edge does not exist yet" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.addEdge ("n1" ~ "n2") 3) `shouldEqual` fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: [ ("n1" ~ "n2") /\ 3 ]
              }
      describe "lookupEdge" do
        describe "start node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.lookupEdge ("n1" ~ "n2")) `shouldEqual` (Left $ inj _startNodeNotFound "n1")
        describe "end node not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.lookupEdge ("n1" ~ "n2")) `shouldEqual` (Left $ inj _endNodeNotFound "n2")
        describe "edge not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.lookupEdge ("n1" ~ "n2")) `shouldEqual` (Left $ inj _edgeNotFound ("n1" ~ "n2"))
        describe "edge found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: [ ("n1" ~ "n2") /\ 3 ]
              }
          it "returns the edge" do
            (graph >>= C.lookupEdge ("n1" ~ "n2")) `shouldEqual` (Right 3)
      describe "deleteEdge" do
        describe "start node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.deleteEdge ("n1" ~ "n2")) `shouldEqual` (Left $ inj _startNodeNotFound "n1")
        describe "end node not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.deleteEdge ("n1" ~ "n2")) `shouldEqual` (Left $ inj _endNodeNotFound "n2")
        describe "edge not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.deleteEdge ("n1" ~ "n2")) `shouldEqual` (Left $ inj _edgeNotFound ("n1" ~ "n2"))
        describe "edge found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: [ ("n1" ~ "n2") /\ 3 ]
              }
          it "deletes the edge" do
            (graph >>= C.deleteEdge ("n1" ~ "n2")) `shouldEqual` fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: []
              }
      describe "updateEdge" do
        describe "start node not found" do
          let
            graph = fromFoldables
              { nodes: []
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.updateEdge ("n1" ~ "n2") 3) `shouldEqual` (Left $ inj _startNodeNotFound "n1")
        describe "end node not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.updateEdge ("n1" ~ "n2") 3) `shouldEqual` (Left $ inj _endNodeNotFound "n2")
        describe "edge not found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: []
              }
          it "returns an error" do
            (graph >>= C.updateEdge ("n1" ~ "n2") 3) `shouldEqual` (Left $ inj _edgeNotFound ("n1" ~ "n2"))
        describe "edge found" do
          let
            graph = fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: [ ("n1" ~ "n2") /\ 0 ]
              }
          it "updates the edge" do
            (graph >>= C.updateEdge ("n1" ~ "n2") 3) `shouldEqual` fromFoldables
              { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
              , edges: [ ("n1" ~ "n2") /\ 3 ]
              }
