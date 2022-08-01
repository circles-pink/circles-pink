module PursTsGen
  ( cla
  , cleanModule
  , constructor
  , defineModules
  , getDuplicates
  , ins
  , module Exp
  , pursModule
  , typeDef
  , value
  )
  where

import Prelude

import Control.Monad.State (State, get, modify, runState)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.String (Pattern(..), Replacement(..))
import Data.String as St
import Data.Traversable (class Foldable, fold, foldr, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PursTsGen.Class.ToTsDef (class GenToTsDefProd, class GenToTsDefSum, class ToTsDef, MyThese(..), genToTsDefProd, genToTsDefSum, genToTsDefSum', genericToTsDef, spec, toTsDef) as Exp
import PursTsGen.Class.ToTsDef (class ToTsDef, toTsDef)
import PursTsGen.Class.ToTsType (class GenRecord, class GenVariant, class ToTsType, genRecord, genVariant, toTsType) as Exp
import PursTsGen.Class.ToTsType (class ToTsType, Constructor(..), toTsType)
import PursTsGen.Lang.TypeScript (Declaration(..), defaultVisitor, rewriteModuleTopDown)
import PursTsGen.Lang.TypeScript.DSL (Declaration(..), Import(..), Module(..), ModuleBody(..), ModuleHead(..), Name(..), Path(..), QualName(..), Type(..), emptyLine, lineComment) as TS
import Type.Proxy (Proxy)



cleanModule :: String -> TS.Module -> TS.Module
cleanModule m = rewriteModuleTopDown defaultVisitor { onType = onType }
  where
  onType = case _ of
    TS.TypeConstructor qn x -> TS.TypeConstructor (cleanQualName qn) x
    TS.TypeOpaque qn x -> TS.TypeOpaque (cleanQualName qn) x
    x -> x

  cleanQualName (TS.QualName (Just x) y) | x == m = TS.QualName Nothing y
  cleanQualName x = x

--------------------------------------------------------------------------------

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
      (y' /\ _) = resolveType y
    in
      DeclValueDef x y'
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

type TypeScope = { quantified :: Array TS.Name, floating :: Array TS.Name }

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

pursModule :: String -> (String /\ String /\ String)
pursModule x = modToAlias x /\ ("../" <> x) /\ x

modToAlias :: String -> String
modToAlias = St.replaceAll (Pattern ".") (Replacement "_")

defineModules :: Map String (String /\ String) -> Array (String /\ Array TS.Declaration) -> Array (String /\ TS.Module)
defineModules mm xs = (\(k /\ v) -> k /\ defineModule mm' k v) <$> xs
  where
  mm' = xs <#> fst >>> pursModule # M.fromFoldable # M.union mm

defineModule :: Map String (String /\ String) -> String -> Array TS.Declaration -> TS.Module
defineModule mm k xs =
  TS.Module moduleHead moduleBody
    # cleanModule (modToAlias k)
  where
  moduleHead = (TS.ModuleHead commentHeader imports)

  imports = xs
    >>= declToRefs
    <#> (\(TS.QualName sc _) -> sc)
    # catMaybes
    # A.nub
    <#> (\key -> (key /\ M.lookup key mm) # sequence)
    # catMaybes
    <#> (\(a /\ p /\ _) -> TS.Import (TS.Name a) (TS.Path p))

  commentHeader =
    [ "Auto generated type signatures."
    , "PureScript Module: " <> k
    ]

  moduleBody = (TS.ModuleBody xs) # resolveModuleBody

declToRefs :: TS.Declaration -> Array TS.QualName
declToRefs = case _ of
  TS.DeclTypeDef _ _ t -> typeToRefs t
  TS.DeclValueDef _ t -> typeToRefs t
  _ -> []

typeToRefs :: TS.Type -> Array TS.QualName
typeToRefs = case _ of
  TS.TypeArray t -> typeToRefs t
  TS.TypeRecord xs -> xs <#> snd >>= typeToRefs
  TS.TypeFunction _ xs r -> (xs <#> snd >>= typeToRefs) <> (typeToRefs r)
  TS.TypeConstructor qn xs -> [ qn ] <> (xs >>= typeToRefs)
  TS.TypeUnion x y -> typeToRefs x <> typeToRefs y
  _ -> []

--------------------------------------------------------------------------------

-- value :: forall a. ToTsType a => String -> a -> Array TS.Declaration
-- value n x =
--   [ TS.emptyLine
--   , TS.lineComment ("Value") -- <> (PT.printType $ toPursType x)
--   , TS.DeclValueDef (TS.Name n) $ toTsType x
--   ]

value :: forall a. ToTsType a => String -> Array TS.Type -> a -> Array TS.Declaration
value n cs x =
  [ TS.emptyLine
  , TS.DeclValueDef (TS.Name n) $ foldr mkFn init cs
  ]
  where
  mkFn y f = TS.TypeFunction S.empty [ (TS.Name "_") /\ y ] f
  init = toTsType x


constructor :: forall a. ToTsType (Constructor a) => String -> a -> Array TS.Declaration
constructor n x = value n [] (Constructor x)

typeDef :: forall a. ToTsDef a => String -> Proxy a -> Array TS.Declaration
typeDef n x =
  [ TS.emptyLine
  , TS.lineComment ("Type")
  , TS.DeclTypeDef (TS.Name n) mempty $ toTsDef x
  ]

cla :: forall dummy a. ToTsDef a => String -> dummy -> Proxy a -> Array TS.Declaration
cla n _ = typeDef n

ins :: TS.Type -> String -> Array TS.Declaration
ins x n =
  [ TS.emptyLine
  , TS.lineComment "Instance"
  , TS.DeclValueDef (TS.Name n) $ x
  ]