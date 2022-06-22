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
        it "an empty graph has no nodeIds" do
          (graph # C.nodeIds) `shouldEqual` S.empty
    describe "outgoingIds"
      let
        graph = C.empty
          # C.insertNode "n1" unit
          # C.insertNode "n2" unit
          # C.insertEdge "n1" "n2" unit
      in
        it ".." do
          (C.outgoingIds "n1" graph) `shouldEqual` (Just $ S.fromFoldable [ "n2" ])
    describe "incomingIds"
      let
        graph = C.empty
          # C.insertNode "n1" unit
          # C.insertNode "n2" unit
          # C.insertEdge "n1" "n2" unit
      in
        it ".." do
          (C.incomingIds "n2" graph) `shouldEqual` (Just $ S.fromFoldable [ "n1" ])
    describe "nodeIds"
      let
        graph = C.empty
          # C.insertNode "n1" unit
          # C.insertNode "n2" unit
      in
        it ".." do
          (C.nodeIds graph) `shouldEqual` (S.fromFoldable [ "n1", "n2" ])