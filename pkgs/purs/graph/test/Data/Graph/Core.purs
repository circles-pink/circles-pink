module Test.Data.Graph.Core
  ( spec
  ) where

import Prelude

import Data.Graph (fromFoldables, toUnfoldables)
import Data.Graph.Core (Graph)
import Data.Graph.Core as C
import Data.Maybe (Maybe(..))
import Data.Pair ((~))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Graph_ = Graph String Unit Unit

spec :: Spec Unit
spec =
  describe "Data.Graph.Core" do
    describe "insertNode"
      let
        graph :: Maybe (Graph String Int Int)
        graph = fromFoldables
          { nodes: []
          , edges: []
          }
      in
        it "inserts a new node" do
          (graph <#> C.insertNode "n1" 1 <#> toUnfoldables) `shouldEqual`
            ( Just
                { nodes: [ "n1" /\ 1 ]
                , edges: []
                }
            )
    describe "attemptDeleteNode"
      let
        graph :: Maybe (Graph String Int Int)
        graph = fromFoldables
          { nodes: [ "n1" /\ 1 ]
          , edges: []
          }
      in
        it "..." do
          (graph <#> C.attemptDeleteNode "n1" <#> toUnfoldables) `shouldEqual`
            ( Just
                { nodes: []
                , edges: []
                }
            )
    describe "lookupNode"
      let
        graph :: Maybe (Graph String Int Int)
        graph = fromFoldables
          { nodes: [ "n1" /\ 1 ]
          , edges: []
          }
      in
        it "..." do
          (graph >>= C.lookupNode "n1") `shouldEqual` (Just 1)
    describe "deleteEdge"
      let
        graph =
          fromFoldables
            { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
            , edges: [ ("n1" ~ "n2") /\ 11 ]
            }
      in
        it "..." do
          (graph <#> C.attemptDeleteEdge ("n1" ~ "n2") <#> toUnfoldables) `shouldEqual`
            ( Just
                { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
                , edges: []
                }
            )
    describe "lookupEdge"
      let
        graph =
          fromFoldables
            { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
            , edges: [ ("n1" ~ "n2") /\ 11 ]
            }
      in
        it "..." do
          (graph >>= C.lookupEdge ("n1" ~ "n2")) `shouldEqual` (Just 11)
    describe "attemptDeleteEdge"
      let
        graph =
          fromFoldables
            { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
            , edges: [ ("n1" ~ "n2") /\ 11 ]
            }
      in
        it "..." do
          (graph <#> C.attemptDeleteEdge ("n1" ~ "n2") <#> toUnfoldables) `shouldEqual`
            ( Just
                { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
                , edges: []
                }
            )
    describe "insertEdge"
      let
        graph =
          fromFoldables
            { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
            , edges: []
            }
      in
        it "..." do
          (graph >>= C.insertEdge ("n1" ~ "n2") 11 <#> toUnfoldables) `shouldEqual`
            ( Just
                { nodes: [ "n1" /\ 1, "n2" /\ 2 ]
                , edges: [ ("n1" ~ "n2") /\ 11 ]
                }
            )
