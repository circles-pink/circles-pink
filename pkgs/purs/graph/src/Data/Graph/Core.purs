module Data.Graph.Core
  ( EitherV
  , Graph
  , GraphSpec
  , addEdge
  , addNode
  , deleteEdge
  , deleteNode
  , edgesToUnfoldable
  , empty
  , foldMapWithIndex
  , foldlWithIndex
  , foldrWithIndex
  , fromFoldables
  , incomingIds
  , lookupEdge
  , lookupNode
  , memberEdge
  , memberNode
  , nodeIds
  , nodesToUnfoldable
  , outgoingIds
  , updateEdge
  , updateNode
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..), isRight, note)
import Data.Foldable (class Foldable, fold, foldM, foldMap, foldl, foldr)
import Data.FoldableWithIndex as F
import Data.Graph.Errors (ErrAddEdge, ErrAddNode, ErrDeleteEdge, ErrDeleteNode, ErrFromFoldables, ErrIncomingIds, ErrLookupEdge, ErrLookupNode, ErrOutgoingIds, ErrUpdateEdge, ErrUpdateNode, _edgeExists, _edgeNotFound, _endNodeNotFound, _nodeExists, _nodeNotFound, _startNodeNotFound)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Pair (Pair, (~))
import Data.Set (Set)
import Data.Set as S
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)
import Data.Variant (Variant, inj)

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype Graph id e n = Graph
  ( Map id
      { data :: n
      , outEdges :: Map id e
      , inIds :: Set id
      }
  )

instance functor :: Functor (Graph id e) where
  map f (Graph nodes) = Graph $ map (\x -> x { data = f x.data }) nodes

instance foldable :: Foldable (Graph id e) where
  foldr f x (Graph nodes) = foldr (\n z -> f n.data z) x nodes
  foldl f x (Graph nodes) = foldl (\z n -> f z n.data) x nodes
  foldMap f (Graph nodes) = foldMap (\n -> f n.data) nodes

derive instance eq :: (Eq id, Eq e, Eq n) => Eq (Graph id e n)

instance show :: (Show id, Show e, Show n) => Show (Graph id e n) where
  show g = "(fromFoldables " <> show (toUnfoldables' g) <> ")"
    where
      toUnfoldables' = toUnfoldables :: _ -> { nodes :: Array _ | _ }

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

type EitherV r a = Either (Variant r) a

type GraphSpec f id e n = { nodes :: f (id /\ n), edges :: f (Pair id /\ e) }

--------------------------------------------------------------------------------
-- Graph API 
--------------------------------------------------------------------------------

empty :: forall id e n. Graph id e n
empty = Graph M.empty

nodeIds :: forall id e n. Graph id e n -> Set id
nodeIds (Graph nodes) = M.keys nodes

foldrWithIndex :: forall id e n z. (id -> n -> z -> z) -> z -> Graph id e n -> z
foldrWithIndex f x (Graph nodes) = F.foldrWithIndex (\id n z -> f id n.data z) x nodes

foldlWithIndex :: forall id e n z. (id -> z -> n -> z) -> z -> Graph id e n -> z
foldlWithIndex f x (Graph nodes) = F.foldlWithIndex (\id z n -> f id z n.data) x nodes

foldMapWithIndex :: forall id e n m. Monoid m => (id -> n -> m) -> Graph id e n -> m
foldMapWithIndex f (Graph nodes) = F.foldMapWithIndex (\id n -> f id n.data) nodes

nodesToUnfoldable :: forall id e n f. Unfoldable f => Graph id e n -> f (id /\ n)
nodesToUnfoldable (Graph nodes) = nodes <#> _.data # M.toUnfoldable

edgesToUnfoldable :: forall id e n f. Unfoldable f => Graph id e n -> f (Pair id /\ e)
edgesToUnfoldable (Graph nodes) = nodes
  # (M.toUnfoldable :: _ -> Array _)
  >>= f
  # A.toUnfoldable
  where
  f (from /\ { outEdges }) = outEdges
    # (M.toUnfoldable :: _ -> Array _)
    <#> (\(to /\ e) -> (from ~ to) /\ e)

fromFoldables :: forall r f id e n. Ord id => Foldable f => GraphSpec f id e n -> EitherV (ErrFromFoldables id r) (Graph id e n)
fromFoldables { nodes: nodes', edges: edges' } = empty
  # (\g -> foldM (flip $ uncurry addNode) g nodes')
  >>= (\g -> foldM (flip $ uncurry addEdge) g edges')

toUnfoldables :: forall id e n f. Unfoldable f => Graph id e n -> GraphSpec f id e n
toUnfoldables g = { nodes: nodesToUnfoldable g, edges: edgesToUnfoldable g }

--------------------------------------------------------------------------------
-- Node API
--------------------------------------------------------------------------------

outgoingIds :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrOutgoingIds id r) (Set id)
outgoingIds id (Graph nodes) = nodes
  # M.lookup id >>> note (inj _nodeNotFound id)
  <#> _.outEdges >>> M.keys

incomingIds :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrIncomingIds id r) (Set id)
incomingIds id (Graph nodes) = nodes
  # M.lookup id >>> note (inj _nodeNotFound id)
  <#> _.inIds

memberNode :: forall id e n. Ord id => id -> Graph id e n -> Boolean
memberNode id g = isRight $ lookupNode id g

--------------------------------------------------------------------------------
-- Node API / CRUD
--------------------------------------------------------------------------------

addNode :: forall r id e n. Ord id => id -> n -> Graph id e n -> EitherV (ErrAddNode id r) (Graph id e n)
addNode id _ g | memberNode id g = Left $ inj _nodeExists id
addNode id node (Graph nodes) = Right $ Graph $ M.alter f id nodes
  where
  f Nothing = Just { data: node, outEdges: M.empty, inIds: S.empty }
  f (Just x) = Just $ x { data = node }

lookupNode :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrLookupNode id r) n
lookupNode id (Graph nodes) = nodes
  # M.lookup id
  <#> _.data
  # (note $ inj _nodeNotFound id)

updateNode :: forall r id e n. Ord id => id -> n -> Graph id e n -> EitherV (ErrUpdateNode id r) (Graph id e n)
updateNode id _ g | not $ memberNode id g = Left $ inj _nodeNotFound id
updateNode id node (Graph nodes) = Right $ Graph $ M.alter f id nodes
  where
  f Nothing = Just { data: node, outEdges: M.empty, inIds: S.empty }
  f (Just x) = Just $ x { data = node }

deleteNode :: forall r id e n. Ord id => id -> Graph id e n -> EitherV (ErrDeleteNode id r) (Graph id e n)
deleteNode id g | not $ memberNode id g = Left $ inj _nodeNotFound id
deleteNode id graph@(Graph nodes) = Right $ Graph $ nodes
  # (\nodes' -> foldr (M.update updateToNode) nodes' outIds)
  # (\nodes' -> foldr (M.update updateFromNode) nodes' inIds)
  # M.delete id
  where
  outIds = fold $ outgoingIds id graph
  inIds = fold $ incomingIds id graph
  updateToNode x = Just $ x { inIds = S.delete id x.inIds }
  updateFromNode x = Just $ x { outEdges = M.delete id x.outEdges }

--------------------------------------------------------------------------------
-- Edge API / CRUD
--------------------------------------------------------------------------------

addEdge :: forall r id e n. Ord id => Pair id -> e -> Graph id e n -> EitherV (ErrAddEdge id r) (Graph id e n)
addEdge (from ~ _) _ g | not $ memberNode from g = Left $ inj _startNodeNotFound from
addEdge (_ ~ to) _ g | not $ memberNode to g = Left $ inj _endNodeNotFound to
addEdge conn _ g | memberEdge conn g = Left $ inj _edgeExists conn
addEdge (from ~ to) edge (Graph nodes) = Right $ Graph $ nodes
  # M.update updateFromNode from
  # M.update updateToNode to
  where
  updateFromNode x = Just $ x { outEdges = M.insert to edge x.outEdges }
  updateToNode x = Just $ x { inIds = S.insert from x.inIds }

lookupEdge :: forall r id e n. Ord id => Pair id -> Graph id e n -> EitherV (ErrLookupEdge id r) e
lookupEdge (_ ~ to) g | not $ memberNode to g = Left $ inj _endNodeNotFound to
lookupEdge conn@(from ~ to) (Graph nodes) = nodes
  # (M.lookup from >>> note (inj _startNodeNotFound from))
  >>= (_.outEdges >>> M.lookup to >>> note (inj _edgeNotFound conn))

deleteEdge :: forall r id e n. Ord id => Pair id -> Graph id e n -> EitherV (ErrDeleteEdge id r) (Graph id e n)
deleteEdge (from ~ _) g | not $ memberNode from g = Left $ inj _startNodeNotFound from
deleteEdge (_ ~ to) g | not $ memberNode to g = Left $ inj _startNodeNotFound to
deleteEdge conn g | not $ memberEdge conn g = Left $ inj _edgeNotFound conn
deleteEdge (from ~ to) (Graph nodes) = Right $ Graph $ nodes
  # M.update updateFromNode from
  # M.update updateToNode to
  where
  updateFromNode x = Just $ x { outEdges = M.delete to x.outEdges }
  updateToNode x = Just $ x { inIds = S.delete from x.inIds }

updateEdge :: forall r id e n. Ord id => Pair id -> e -> Graph id e n -> EitherV (ErrUpdateEdge id r) (Graph id e n)
updateEdge (from ~ _) _ g | not $ memberNode from g = Left $ inj _startNodeNotFound from
updateEdge (_ ~ to) _ g | not $ memberNode to g = Left $ inj _endNodeNotFound to
updateEdge conn _ g | not $ memberEdge conn g = Left $ inj _edgeNotFound conn
updateEdge (from ~ to) edge (Graph nodes) = Right $ Graph $ nodes
  # M.update updateFromNode from
  # M.update updateToNode to
  where
  updateFromNode x = Just $ x { outEdges = M.insert to edge x.outEdges }
  updateToNode x = Just $ x { inIds = S.insert from x.inIds }

--------------------------------------------------------------------------------
-- Edge API
--------------------------------------------------------------------------------

memberEdge :: forall id e n. Ord id => Pair id -> Graph id e n -> Boolean
memberEdge conn g = isRight $ lookupEdge conn g