module Language.Dot.Graph.Print where

import Prelude
import Data.Array (intercalate)
import Data.Array as A
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as S
import Data.Variant (case_, onMatch)
import Language.Dot.Attr (Attr(..))
import Language.Dot.Graph (AttrStmt, ClusterSubGraph(..), EdgeEnd, EdgeStmt(..), Graph(..), NodeId(..), NodeStmt(..), Stmt, GraphType, clusterSubGraph)
import Language.Dot.Id (Id(..))
import Type.Proxy (Proxy(..))
import Undefined (undefined)

type Matrix
  = Array (Array String)

printGraph :: Graph -> String
printGraph g = (S.joinWith "\n" <<< map (S.joinWith " ")) $ printGraph' g

printGraph' :: Graph -> Matrix
printGraph' (Graph g) =
  join
    [ [ join [ strict g.strict, [ graphType g.type ], getId g.id, [ "{" ] ]
      ]
    , join (map (printStmt g.type >>> indentMatrix) g.stmts)
    , [ [ "}" ]
      ]
    ]
  where
  strict b = if b then [ "strict" ] else []

  graphType =
    case_
      # onMatch
          { directed: \_ -> "digraph"
          , undirected: \_ -> "graph"
          }

  getId Nothing = []

  getId (Just id) = [ printId id ]

printStmt :: GraphType -> Stmt -> Matrix
printStmt gt =
  case_
    # onMatch
        { "clusterSubGraph": \x -> printClusterSubGraph gt x
        , "nodeStmt": \x -> printNodeStmt x
        , "edgeStmt": \x -> printEdgeStmt gt x
        , "attrStmt": \x -> printAttrStmt x
        }

printClusterSubGraph :: GraphType -> ClusterSubGraph -> Matrix
printClusterSubGraph gt (ClusterSubGraph { id, stmts }) =
  join
    [ [ join [ [ "subgraph" ], getId id, [ "{" ] ] ]
    , join (map (\x -> printStmt gt x # indentMatrix) stmts)
    , [ [ "}" ] ]
    ]
  where
  getId Nothing = []

  getId (Just id) = [ "cluster_" <> printId id ]

printNodeStmt :: NodeStmt -> Matrix
printNodeStmt (NodeStmt { id, attrs }) =
  join
    [ [ [ printNodeId id, "[" ] ]
    , join (map (\x -> printAttrb x # indentMatrix) attrs)
    , [ [ "]" ] ]
    ]

printEdgeStmt :: GraphType -> EdgeStmt -> Matrix
printEdgeStmt gt (EdgeStmt source target targets attrs) =
  join
    [ [ printEdgeEnds gt ([ source, target ] <> targets) <> [ "[" ] ]
    , join (map (\x -> printAttrb x # indentMatrix) attrs)
    , [ [ "]" ] ]
    ]

printEdgeEnds :: GraphType -> Array EdgeEnd -> Array String
printEdgeEnds gt edgeEnds = edgeEnds <#> printEdgeEnd # A.intersperse (printConnec gt)

printConnec :: GraphType -> String
printConnec =
  case_
    # onMatch
        { "directed": \_ -> "->"
        , "undirected": \_ -> "--"
        }

printEdgeEnd :: EdgeEnd -> String
printEdgeEnd = case_ # onMatch { "nodeId": printNodeId }

printAttrStmt :: AttrStmt -> Matrix
printAttrStmt =
  case_
    # onMatch
        { "cluster":
            \attrs ->
              join
                [ [ [ "graph", "[" ] ]
                , join $ map (\x -> printAttrb x # indentMatrix) attrs
                , [ [ "]" ] ]
                ]
        }

printAttrb :: forall a. Attr a -> Matrix
printAttrb (Attr { key, value }) = [ [ printId key, "=", joinWith "" [ "\"", printId value, "\"" ], ";" ] ]

printId :: Id -> String
printId (Id id) = id

printNodeId :: NodeId -> String
printNodeId (NodeId { id }) = printId id

indentMatrix :: Matrix -> Matrix
indentMatrix m = map (\l -> [ "", "" ] <> l) m
