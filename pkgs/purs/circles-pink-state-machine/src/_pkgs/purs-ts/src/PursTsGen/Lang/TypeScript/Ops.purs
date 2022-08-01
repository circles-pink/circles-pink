module PursTsGen.Lang.TypeScript.Ops where

import PursTsGen.Prelude

import Control.Monad.State (State, get, modify, runState)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as M
import Data.Set as S
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PursTsGen.Lang.TypeScript (Declaration(..), defaultVisitor)
import PursTsGen.Lang.TypeScript.DSL (Declaration, ModuleBody(..), Name, Type(..)) as TS
import PursTsGen.Lang.TypeScript.Traversal (rewriteTypeTopDown)

type TypeScope = { quantified :: Array TS.Name, floating :: Array TS.Name }

resolveModuleBody :: TS.ModuleBody -> TS.ModuleBody
resolveModuleBody (TS.ModuleBody xs) = TS.ModuleBody $ resolveDeclaration <$> xs

resolveDeclaration :: TS.Declaration -> TS.Declaration
resolveDeclaration = case _ of
  DeclTypeDef x _ y ->
    let
      (y' /\ { floating }) = resolveType y
    in
      DeclTypeDef x floating y' -- todo
  DeclValueDef x y ->
    let
      (y' /\ { floating }) = resolveType y
    in
      DeclValueDef x $ unquantifiableToNever (Set.fromFoldable floating) y'
  x -> x

unquantifiableToNever :: Set TS.Name -> TS.Type -> TS.Type
unquantifiableToNever ns = rewriteTypeTopDown defaultVisitor { onType = onType }
  where
  onType = case _ of
    TS.TypeVar n | Set.member n ns -> TS.TypeNull
    x -> x

resolveType :: TS.Type -> TS.Type /\ TypeScope
resolveType x = runState (resolveType' x) mempty

resolveType' :: TS.Type -> State TypeScope TS.Type
resolveType' = case _ of
  TS.TypeArray t -> TS.TypeArray <$> resolveType' t
  TS.TypeRecord xs -> TS.TypeRecord <$> traverse (traverse resolveType') xs
  TS.TypeFunction _ xs r -> resolveFunction xs r
  TS.TypeVar n -> resolveVar n
  TS.TypeConstructor qn xs -> TS.TypeConstructor qn <$> traverse resolveType' xs
  TS.TypeOpaque x ns -> resolveOpaque x ns
  TS.TypeUnion x y -> TS.TypeUnion <$> resolveType' x <*> resolveType' y
  x -> pure x
  where

  resolveOpaque x ns = do
    _ <- modify (\s -> s { floating = ns <> s.floating })
    pure $ TS.TypeOpaque x ns

  resolveVar n = do
    _ <- modify (\s -> s { floating = A.snoc s.floating n })
    pure $ TS.TypeVar n

  combine :: forall a b f. Functor f => Foldable f => Monoid b => f (a /\ b) -> (f a) /\ b
  combine xs = map fst xs /\ (fold $ map snd xs)

  resolveFunction :: Array (TS.Name /\ TS.Type) -> TS.Type -> State TypeScope TS.Type
  resolveFunction xs r = do
    st <- get

    let f (x /\ y) = runState (resolveType' y) st # lmap (\y' -> x /\ y')

    let r' /\ stR = runState (resolveType' r) st
    let xs' /\ stXs = combine $ (map f xs)

    let { floating, quantified } = stR <> stXs
    let deleteBelow = getDuplicates (quantified <> floating) `S.union` (S.fromFoldable floating)

    _ <- modify \s -> s
      { floating = mempty :: Array TS.Name
      , quantified = A.nub (quantified <> floating)
      }

    pure $ TS.TypeFunction (S.fromFoldable floating `S.union` deleteBelow)
      (map (deleteQuant deleteBelow) <$> xs')
      (deleteQuant deleteBelow r')

--------------------------------------------------------------------------------

getDuplicates :: forall a. Ord a => Array a -> Set a
getDuplicates xs =
  let
    dupLookup :: Map a Boolean
    dupLookup = xs <#> (flip Tuple false) # M.fromFoldable

    f :: a -> (Set a /\ Map a Boolean) -> (Set a /\ Map a Boolean)
    f x (out /\ lu) | M.lookup x lu == Just false = out /\ M.insert x true lu
    f x (out /\ lu) | M.lookup x lu == Just true = S.insert x out /\ lu
    f _ _ | otherwise = unsafePartial $ unsafeCrashWith "filtered out above"

  in
    xs
      # foldr f ((S.empty :: Set a) /\ dupLookup)
      # fst

--------------------------------------------------------------------------------

deleteQuant :: Set TS.Name -> TS.Type -> TS.Type
deleteQuant s = case _ of
  TS.TypeArray t -> TS.TypeArray $ deleteQuant s t
  TS.TypeRecord xs -> TS.TypeRecord $ map (map $ deleteQuant s) xs
  TS.TypeFunction ta xs r -> deleteQuantFunction ta xs r
  TS.TypeConstructor qn xs -> TS.TypeConstructor qn $ map (deleteQuant s) xs
  TS.TypeUnion x y -> TS.TypeUnion (deleteQuant s x) (deleteQuant s y)
  x -> x
  where

  deleteQuantFunction ta xs r = TS.TypeFunction
    (foldr S.delete ta (S.intersection ta s))
    (map (map $ deleteQuant s) xs)
    (deleteQuant s r)

