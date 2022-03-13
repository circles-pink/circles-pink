module Language.Dot.Graph.Print where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as S
import Data.Variant (case_, onMatch)
import Language.Dot.Attr (Attr(..))
import Language.Dot.Graph (ClusterSubGraph(..), Graph(..), NodeId(..), NodeStmt(..), Stmt, AttrStmt, clusterSubGraph)
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
    , join (map (printStmt >>> indentMatrix) g.stmts)
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

printStmt :: Stmt -> Matrix
printStmt =
  case_
    # onMatch
        { "clusterSubGraph": \x -> printClusterSubGraph x
        , "nodeStmt": \x -> printNodeStmt x
        , "attrStmt": \x -> printAttrStmt x
        }

printClusterSubGraph :: ClusterSubGraph -> Matrix
printClusterSubGraph (ClusterSubGraph { id, stmts }) =
  join
    [ [ join [ [ "subgraph" ], getId id, [ "{" ] ] ]
    , join (map (\x -> printStmt x # indentMatrix) stmts)
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
