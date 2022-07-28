module Test.Data.Graph
  ( spec
  ) where

import Prelude

import Data.Array (sort)
import Data.Either (Either(..))
import Data.Graph (EitherV, Graph, GraphSpec, NeighborConnectivity(..))
import Data.Graph as G
import Data.Graph.Errors (ErrAll)
import Data.Pair ((~))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type TestGraph = Graph GraphId GraphEdge GraphNode

type GraphId = String

type GraphEdge = Char

type GraphNode = Int

spec :: Spec Unit
spec =
  let
    -- | Monomorphic `fromFoldables` for better type inference in tests
    fromFoldables :: GraphSpec Array GraphId GraphEdge GraphNode -> EitherV (ErrAll GraphId ()) TestGraph
    fromFoldables = G.fromFoldables
  in
    describe "Data.Graph" do
      describe "neighborhood" do
        let
          graph = fromFoldables
            { nodes: [ "n0" /\ 0, "n1" /\ 1, "n2" /\ 2, "n3" /\ 3, "n4" /\ 4 ]
            , edges:
                [ ("n0" ~ "n1") /\ 'a'
                , ("n0" ~ "n2") /\ 'b'
                , ("n2" ~ "n0") /\ 'c'
                , ("n3" ~ "n0") /\ 'd'
                ]
            }
        it "returns an array of correct neighbor connections" do
          (graph >>= G.neighborhood "n0" <#> sort) `shouldEqual`
            ( Right $ sort
                [ JustOutgoing 'a' /\ ("n1" /\ 1)
                , MutualOutAndIn 'b' 'c' /\ ("n2" /\ 2)
                , JustIncoming 'd' /\ ("n3" /\ 3)
                ]
            )
