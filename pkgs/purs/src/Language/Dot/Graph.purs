module Language.Dot.Graph
  ( AttrStmt
  , ClusterSubGraph(..)
  , CompassPt
  , EdgeEnd
  , EdgeStmt(..)
  , Graph(..)
  , GraphType
  , NodeId(..)
  , NodeStmt(..)
  , Port(..)
  , Stmt
  , attrStmt
  , cluster
  , clusterSubGraph
  , directed_
  , edge
  , edgeStmt
  , graph
  , nodeId
  , nodeStmt
  , undirected_
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Language.Dot.Attr (Attr, C, E, G, N)
import Language.Dot.Id (Id(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Graph
  = Graph
  { strict :: Boolean
  , type :: GraphType
  , id :: Maybe Id
  , stmts :: Array Stmt
  }

type GraphType
  = Variant ( directed :: Unit, undirected :: Unit )

directed_ :: forall v. Variant ( directed :: Unit | v )
directed_ = inj (Proxy :: _ "directed") unit

undirected_ :: forall v. Variant ( undirected :: Unit | v )
undirected_ = inj (Proxy :: _ "undirected") unit

nodeStmt :: forall a v. a -> Variant ( nodeStmt :: a | v )
nodeStmt = inj (Proxy :: _ "nodeStmt")

edgeStmt :: forall a v. a -> Variant ( edgeStmt :: a | v )
edgeStmt = inj (Proxy :: _ "edgeStmt")

attrStmt :: forall a v. a -> Variant ( attrStmt :: a | v )
attrStmt = inj (Proxy :: _ "attrStmt")

cluster :: forall a v. a -> Variant ( cluster :: a | v )
cluster = inj (Proxy :: _ "cluster")

graph :: forall a v. a -> Variant ( graph :: a | v )
graph = inj (Proxy :: _ "graph")

edge :: forall a v. a -> Variant ( edge :: a | v )
edge = inj (Proxy :: _ "edge")

nodeId :: forall a v. a -> Variant ( nodeId :: a | v )
nodeId = inj (Proxy :: _ "nodeId")

clusterSubGraph :: forall a v. a -> Variant ( clusterSubGraph :: a | v )
clusterSubGraph = inj (Proxy :: _ "clusterSubGraph")

type Stmt
  = Variant
      ( nodeStmt :: NodeStmt
      , edgeStmt :: EdgeStmt
      , attrStmt :: AttrStmt
      , clusterSubGraph :: ClusterSubGraph
      --, subGraph :: SubGraph
      )

data ClusterSubGraph
  = ClusterSubGraph { id :: Maybe Id, stmts :: Array Stmt }

type AttrStmt
  = ( Variant
        ( cluster :: Array (Attr C)
        , graph :: Array (Attr G)
        --, node :: Unit
        , edge :: Array (Attr E)
        )
    )

newtype NodeId
  = NodeId
  { id :: Id
  --, port :: Maybe Port
  }

data Port
  = Port Id (Maybe CompassPt)

type CompassPt
  = Variant
      ( n :: Unit
      , ne :: Unit
      , e :: Unit
      , se :: Unit
      , s :: Unit
      , sw :: Unit
      , w :: Unit
      , nw :: Unit
      , c :: Unit
      , "_" :: Unit
      )

newtype NodeStmt
  = NodeStmt { id :: NodeId, attrs :: Array (Attr N) }

type EdgeEnd
  = Variant
      ( nodeId :: NodeId
      -- , subgraph :: SubGraph
      )

data EdgeStmt
  = EdgeStmt EdgeEnd EdgeEnd (Array EdgeEnd) (Array (Attr E))

--------------------------------------------------------------------------------
-- class Stringify
--------------------------------------------------------------------------------
class Stringify a where
  toString :: a -> String
