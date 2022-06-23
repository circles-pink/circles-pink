module Test.Data.Graph.Core
  ( spec
  ) where

import Prelude

import Data.Graph.Core (Graph)
import Data.Graph.Core as C
import Data.Maybe (Maybe(..))
import Data.Set as S
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Graph_ = Graph String Unit Unit

spec :: Spec Unit
spec =
  describe "Data.Graph.Core" do
    describe "empty"
      let
        graph :: Graph_
        graph = C.empty
      in
        it "retrieves no node ids" do
          (graph # C.nodeIds) `shouldEqual` S.empty
    describe "outgoingIds"
      let
        graph = C.empty
          # C.insertNode "n1" unit
          # C.insertNode "n2" unit
          # C.insertEdge "n1" "n2" unit
      in
        it "retrieves all outgoing ids at a given id" do
          (C.outgoingIds "n1" graph) `shouldEqual` (Just $ S.fromFoldable [ "n2" ])
    describe "incomingIds"
      let
        graph = C.empty
          # C.insertNode "n1" unit
          # C.insertNode "n2" unit
          # C.insertEdge "n1" "n2" unit
      in
        it "retrieves all imcoming ids at a given id" do
          (C.incomingIds "n2" graph) `shouldEqual` (Just $ S.fromFoldable [ "n1" ])
    describe "nodeIds"
      let
        graph = C.empty
          # C.insertNode "n1" unit
          # C.insertNode "n2" unit
      in
        it "retrieves all node ids in the graph" do
          (C.nodeIds graph) `shouldEqual` (S.fromFoldable [ "n1", "n2" ])
    describe "lookupNode"
      let
        graph = C.empty
          # C.insertNode "n1" 1
          # C.insertNode "n2" 2
      in
        it "retrieves the node at the given id" do
          (C.lookupNode "n1" graph) `shouldEqual` (Just 1)
    describe "lookupEdge"
      let
        graph = C.empty
          # C.insertNode "n1" 1
          # C.insertNode "n2" 2
          # C.insertEdge "n1" "n2" 11
      in
        it "retrieves the edge at from the given source and target ids" do
          (C.lookupEdge "n1" "n2" graph ) `shouldEqual` (Just 11)
    -- describe "insertNode"
    --   let
    --     graph = C.empty
    --   in
    --     it "it" do
    --       (C.insertNode "n1" graph # C. ) `shouldEqual` 2