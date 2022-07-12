module Pursts
  ( cleanModule
  , getDuplicates
  , resolveModule
  ) where

import Prelude

import Control.Monad.State (class MonadState, State, get, modify, runState)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (class Foldable, class Traversable, fold, foldr, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Extra (todo)
import Language.TypeScript.DTS (Declaration(..))
import Language.TypeScript.DTS as DTS
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

class Clean a where
  clean :: String -> a -> a

instance cleanModule' :: Clean (DTS.Module a) where
  clean m (DTS.Module xs) = DTS.Module $ clean m <$> xs

instance cleanDeclaration :: Clean (DTS.Declaration a) where
  clean m x = case x of
    DTS.DeclTypeDef x' y t -> DTS.DeclTypeDef x' y $ clean m t
    DTS.DeclValueDef x' t -> DTS.DeclValueDef x' $ clean m t
    _ -> x

instance cleanType :: Clean (DTS.Type a) where
  clean m = case _ of
    DTS.TypeString -> DTS.TypeString
    DTS.TypeNumber -> DTS.TypeNumber
    DTS.TypeBoolean -> DTS.TypeBoolean
    DTS.TypeArray t -> DTS.TypeArray $ clean m t
    DTS.TypeRecord xs -> DTS.TypeRecord $ (map $ clean m) <$> xs
    DTS.TypeFunction xs ys t -> DTS.TypeFunction xs ((map $ clean m) <$> ys) (clean m t)
    DTS.TypeVar x -> DTS.TypeVar x
    DTS.TypeConstructor qn x -> DTS.TypeConstructor (clean m qn) (clean m <$> x)
    DTS.TypeOpaque x -> DTS.TypeOpaque x
    DTS.TypeUnion xs -> DTS.TypeUnion $ clean m <$> xs
    DTS.TypeTLString s -> DTS.TypeTLString s

instance cleanQualName :: Clean DTS.QualName where
  clean m (DTS.QualName (Just x) y) | x == m = DTS.QualName Nothing y
  clean _ all = all

cleanModule :: forall a. String -> DTS.Module a -> DTS.Module a
cleanModule = clean

resolveModule :: DTS.Module Unit -> DTS.Module (Set DTS.Name)
resolveModule (DTS.Module xs) = DTS.Module $ resolveDeclaration <$> xs

resolveDeclaration :: DTS.Declaration Unit -> DTS.Declaration (Set DTS.Name)
resolveDeclaration = case _ of
  DeclTypeDef x _ y ->
    let
      (y' /\ { floating }) = resolveType y
    in
      DeclTypeDef x (S.fromFoldable floating) y' -- todo
  DeclValueDef x y ->
    let
      (y' /\ _) = resolveType y
    in
      DeclValueDef x y'
  DeclImport x y -> DeclImport x y

resolveType :: DTS.Type Unit -> DTS.Type (Set DTS.Name) /\ TypeScope
resolveType x = runState (resolveType' x) mempty

resolveType' :: DTS.Type Unit -> State TypeScope (DTS.Type (Set DTS.Name))
resolveType' = case _ of
  DTS.TypeString -> pure DTS.TypeString
  DTS.TypeNumber -> pure DTS.TypeNumber
  DTS.TypeBoolean -> pure DTS.TypeBoolean
  DTS.TypeArray t -> DTS.TypeArray <$> resolveType' t
  DTS.TypeRecord xs -> DTS.TypeRecord <$> traverse (traverse resolveType') xs
  DTS.TypeFunction _ xs r -> resolveFunction xs r
  DTS.TypeVar n -> resolveVar n
  DTS.TypeConstructor qn xs -> DTS.TypeConstructor qn <$> traverse resolveType' xs
  DTS.TypeOpaque ns -> resolveOpaque ns
  DTS.TypeUnion xs -> DTS.TypeUnion <$> traverse resolveType' xs
  DTS.TypeTLString s -> pure $ DTS.TypeTLString s
  where

  resolveOpaque ns = do
    _ <- modify (\s -> s { floating = ns <> s.floating  })
    pure $ DTS.TypeOpaque ns

  resolveVar n = do
    _ <- modify (\s -> s { floating = A.snoc s.floating n })
    pure $ DTS.TypeVar n

  combine :: forall a b f. Functor f => Foldable f => Monoid b => f (a /\ b) -> (f a) /\ b
  combine xs = map fst xs /\ (fold $ map snd xs)

  resolveFunction :: Array (DTS.Name /\ DTS.Type Unit) -> DTS.Type Unit -> State TypeScope (DTS.Type (Set DTS.Name))
  resolveFunction xs r = do
    st <- get

    let f (x /\ y) = runState (resolveType' y) st # lmap (\y' -> x /\ y')

    let r' /\ stR = runState (resolveType' r) st
    let xs' /\ stXs = combine $ (map f xs)

    let { floating, quantified } = stR <> stXs
    let deleteBelow = getDuplicates (quantified <> floating) `S.union` (S.fromFoldable floating)

    _ <- modify \s -> s
      { floating = mempty :: Array DTS.Name
      , quantified = A.nub (quantified <> floating)
      }

    pure $ DTS.TypeFunction (S.fromFoldable floating `S.union` deleteBelow)
      (map (deleteQuant deleteBelow) <$> xs')
      (deleteQuant deleteBelow r')

deleteQuant :: Set DTS.Name -> DTS.Type (Set DTS.Name) -> DTS.Type (Set DTS.Name)
deleteQuant s = case _ of
  DTS.TypeString -> DTS.TypeString
  DTS.TypeNumber -> DTS.TypeNumber
  DTS.TypeBoolean -> DTS.TypeBoolean
  DTS.TypeArray t -> DTS.TypeArray $ deleteQuant s t
  DTS.TypeRecord xs -> DTS.TypeRecord $ map (map $ deleteQuant s) xs
  DTS.TypeFunction ta xs r -> DTS.TypeFunction
    (foldr S.delete ta (S.intersection ta s))
    (map (map $ deleteQuant s) xs)
    (deleteQuant s r)
  DTS.TypeVar n -> DTS.TypeVar n
  DTS.TypeConstructor qn xs -> DTS.TypeConstructor qn $ map (deleteQuant s) xs
  DTS.TypeOpaque x -> DTS.TypeOpaque x
  DTS.TypeUnion xs -> DTS.TypeUnion $ map (deleteQuant s) xs
  DTS.TypeTLString str -> DTS.TypeTLString str

type TypeScope = { quantified :: Array DTS.Name, floating :: Array DTS.Name }

getDuplicates :: forall a. Ord a => Array a -> Set a
getDuplicates xs =
  let
    dupLookup :: Map a Boolean
    dupLookup = xs <#> (flip Tuple false) # M.fromFoldable

    f :: a -> (Set a /\ Map a Boolean) -> (Set a /\ Map a Boolean)
    f x (out /\ lu) | M.lookup x lu == Just false = out /\ M.insert x true lu
    f x (out /\ lu) | M.lookup x lu == Just true = S.insert x out /\ lu
    f x (out /\ lu) | otherwise = unsafePartial $ unsafeCrashWith "filtered out above"

  in
    xs
      # foldr f ((S.empty :: Set a) /\ dupLookup)
      # fst

