module Language.Dot.Graph
  ( CompassPt
  , EdgeEnd
  , EdgeStmt(..)
  , Graph(..)
  , NodeId(..)
  , NodeStmt(..)
  , Port(..)
  , Stmt
  , SubGraph(..)
  , directed_
  , nodeStmt
  , undirected_
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Language.Dot.Attr (Attr, E, N)
import Language.Dot.Id (Id(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Graph
  = Graph
  { strict :: Boolean
  , type :: Variant ( directed :: Unit, undirected :: Unit )
  , id :: Maybe Id
  , stmts :: Array Stmt
  }

directed_ :: forall v. Variant ( directed :: Unit | v )
directed_ = inj (Proxy :: _ "directed") unit

undirected_ :: forall v. Variant ( undirected :: Unit | v )
undirected_ = inj (Proxy :: _ "undirected") unit

nodeStmt :: forall a v. a -> Variant ( nodeStmt :: a | v )
nodeStmt = inj (Proxy :: _ "nodeStmt")

type Stmt
  = Variant
      ( nodeStmt :: NodeStmt
      , edgeStmt :: EdgeStmt
      --, attrStmt :: AttrStmt
      , subGraph :: SubGraph
      )

data SubGraph
  = SubGraph { id :: Id, stmts :: Array Stmt }

-- data AttrStmt
--   = AttrStmt
--     ( Variant
--         ( graph :: Unit
--         , node :: Unit
--         , edge :: Unit
--         )
--     )
--     (Array (Attr a))
newtype NodeId
  = NodeId { id :: Id, port :: Maybe Port }

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
      , subgraph :: SubGraph
      )

data EdgeStmt
  = EdgeStmt EdgeEnd EdgeEnd (Array EdgeEnd) (Array (Attr E))

--------------------------------------------------------------------------------
-- class Stringify
--------------------------------------------------------------------------------
class Stringify a where
  toString :: a -> String
