module Test.Data.Graph.Core
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Graph.Core (Graph, EitherV, GraphSpec)
import Data.Graph.Core as C
import Data.Graph.Errors (ErrAll, _nodeExists, _nodeNotFound)
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
-- describe "insertNode"
--   let
--     graph :: Maybe (Graph String Int Int)
--     graph = fromFoldables
--       { nodes: []
--       , edges: []
--       }
--   in
--     it "inserts a new node" do
--       (graph <#> C.insertNode "n1" 1 <#> toUnfoldables) `shouldEqual`
--         ( Just
--             { nodes: [ "n1" /\ 1 ]
--             , edges: []
--             }
--         )
-- describe "attemptDeleteNode"
--   let
--     graph :: Maybe (Graph String Int Int)
--     graph = fromFoldables
--       { nodes: [ "n1" /\ 1 ]
--       , edges: []
--       }
--   in
--     it "..." do
--       (graph <#> C.attemptDeleteNode "n1" <#> toUnfoldables) `shouldEqual`
--         ( Just
--             { nodes: []
--             , edges: []
--             }
--         )
-- describe "lookupNode"
--   let
--     graph :: Maybe (Graph String Int Int)
--     graph = fromFoldables
--       { nodes: [ "n1" /\ 1 ]
--       , edges: []
--       }
--   in
--     it "..." do
--       (graph >>= C.lookupNode "n1") `shouldEqual` (Just 1)
-- describe "deleteEdge"
--   let
--     graph =
--       fromFoldables
--         { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--         , edges: [ ("n1" ~ "n2") /\ 11 ]
--         }
--   in
--     it "..." do
--       (graph <#> C.attemptDeleteEdge ("n1" ~ "n2") <#> toUnfoldables) `shouldEqual`
--         ( Just
--             { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--             , edges: []
--             }
--         )
-- describe "lookupEdge"
--   let
--     graph =
--       fromFoldables
--         { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--         , edges: [ ("n1" ~ "n2") /\ 11 ]
--         }
--   in
--     it "..." do
--       (graph >>= C.lookupEdge ("n1" ~ "n2")) `shouldEqual` (Just 11)
-- describe "attemptDeleteEdge"
--   let
--     graph =
--       fromFoldables
--         { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--         , edges: [ ("n1" ~ "n2") /\ 11 ]
--         }
--   in
--     it "..." do
--       (graph <#> C.attemptDeleteEdge ("n1" ~ "n2") <#> toUnfoldables) `shouldEqual`
--         ( Just
--             { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--             , edges: []
--             }
--         )
-- describe "insertEdge"
--   let
--     graph =
--       fromFoldables
--         { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--         , edges: []
--         }
--   in
--     it "..." do
--       (graph >>= C.insertEdge ("n1" ~ "n2") 11 <#> toUnfoldables) `shouldEqual`
--         ( Just
--             { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
--             , edges: [ ("n1" ~ "n2") /\ 11 ]
--             }
--         )
