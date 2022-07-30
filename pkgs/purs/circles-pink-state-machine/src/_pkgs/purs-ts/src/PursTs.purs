module PursTs
  ( cleanModule
  , defineModules
  , getDuplicates
  , pursModule
  ) where

import Prelude

import Control.Monad.State (State, get, modify, runState)
import Data.Array (catMaybes)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as S
import Data.String (Pattern(..), Replacement(..))
import Data.String as St
import Data.Traversable (class Foldable, fold, foldr, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Language.TypeScript.DTS (Declaration(..))
import Language.TypeScript.DTS (Declaration(..), Import(..), Module(..), ModuleBody(..), ModuleHead(..), Name(..), Path(..), QualName(..), Type(..)) as DTS
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

--import PursTs.Class (class ToTsDef, class ToTsType, toTsDef, toTsType)

class Clean a where
  clean :: String -> a -> a

instance cleanModule' :: Clean DTS.Module where
  clean m (DTS.Module mh mb) = DTS.Module mh $ clean m mb

instance cleanModuleBody :: Clean DTS.ModuleBody where
  clean m (DTS.ModuleBody ds) = DTS.ModuleBody $ clean m <$> ds

instance cleanDeclaration :: Clean DTS.Declaration where
  clean m x = case x of
    DTS.DeclTypeDef x' y t -> DTS.DeclTypeDef x' y $ clean m t
    DTS.DeclValueDef x' t -> DTS.DeclValueDef x' $ clean m t
    t -> t

instance cleanType :: Clean DTS.Type where
  clean m = case _ of
    DTS.TypeNull -> DTS.TypeNull
    DTS.TypeUndefined -> DTS.TypeUndefined
    DTS.TypeString -> DTS.TypeString
    DTS.TypeNumber -> DTS.TypeNumber
    DTS.TypeBoolean -> DTS.TypeBoolean
    DTS.TypeArray t -> DTS.TypeArray $ clean m t
    DTS.TypeRecord xs -> DTS.TypeRecord $ (map $ clean m) <$> xs
    DTS.TypeFunction xs ys t -> DTS.TypeFunction xs ((map $ clean m) <$> ys) (clean m t)
    DTS.TypeVar x -> DTS.TypeVar x
    DTS.TypeConstructor qn x -> DTS.TypeConstructor (clean m qn) (clean m <$> x)
    DTS.TypeOpaque y x -> DTS.TypeOpaque y x
    DTS.TypeUnion x y -> DTS.TypeUnion (clean m x) (clean m y)
    DTS.TypeTLString s -> DTS.TypeTLString s

instance cleanQualName :: Clean DTS.QualName where
  clean m (DTS.QualName (Just x) y) | x == m = DTS.QualName Nothing y
  clean _ all = all

cleanModule :: String -> DTS.Module -> DTS.Module
cleanModule = clean

--------------------------------------------------------------------------------

-- resolveModule :: DTS.Module Unit -> DTS.Module (Set DTS.Name)
-- resolveModule (DTS.Module mh mb) = DTS.Module mh $ resolveModuleBody mb

resolveModuleBody :: DTS.ModuleBody -> DTS.ModuleBody
resolveModuleBody (DTS.ModuleBody xs) = DTS.ModuleBody $ resolveDeclaration <$> xs

resolveDeclaration :: DTS.Declaration -> DTS.Declaration
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

resolveType :: DTS.Type -> DTS.Type /\ TypeScope
resolveType x = runState (resolveType' x) mempty

resolveType' :: DTS.Type -> State TypeScope DTS.Type
resolveType' = case _ of
  DTS.TypeNull -> pure DTS.TypeNull
  DTS.TypeUndefined -> pure DTS.TypeUndefined
  DTS.TypeString -> pure DTS.TypeString
  DTS.TypeNumber -> pure DTS.TypeNumber
  DTS.TypeBoolean -> pure DTS.TypeBoolean
  DTS.TypeArray t -> DTS.TypeArray <$> resolveType' t
  DTS.TypeRecord xs -> DTS.TypeRecord <$> traverse (traverse resolveType') xs
  DTS.TypeFunction _ xs r -> resolveFunction xs r
  DTS.TypeVar n -> resolveVar n
  DTS.TypeConstructor qn xs -> DTS.TypeConstructor qn <$> traverse resolveType' xs
  DTS.TypeOpaque x ns -> resolveOpaque x ns
  DTS.TypeUnion x y -> DTS.TypeUnion <$> resolveType' x <*> resolveType' y
  DTS.TypeTLString s -> pure $ DTS.TypeTLString s
  where

  resolveOpaque x ns = do
    _ <- modify (\s -> s { floating = ns <> s.floating })
    pure $ DTS.TypeOpaque x ns

  resolveVar n = do
    _ <- modify (\s -> s { floating = A.snoc s.floating n })
    pure $ DTS.TypeVar n

  combine :: forall a b f. Functor f => Foldable f => Monoid b => f (a /\ b) -> (f a) /\ b
  combine xs = map fst xs /\ (fold $ map snd xs)

  resolveFunction :: Array (DTS.Name /\ DTS.Type) -> DTS.Type -> State TypeScope DTS.Type
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

--------------------------------------------------------------------------------

deleteQuant :: Set DTS.Name -> DTS.Type -> DTS.Type
deleteQuant s = case _ of
  DTS.TypeNull -> DTS.TypeNull
  DTS.TypeUndefined -> DTS.TypeUndefined
  DTS.TypeString -> DTS.TypeString
  DTS.TypeNumber -> DTS.TypeNumber
  DTS.TypeBoolean -> DTS.TypeBoolean
  DTS.TypeArray t -> DTS.TypeArray $ deleteQuant s t
  DTS.TypeRecord xs -> DTS.TypeRecord $ map (map $ deleteQuant s) xs
  DTS.TypeFunction ta xs r -> deleteQuantFunction ta xs r
  DTS.TypeVar n -> DTS.TypeVar n
  DTS.TypeConstructor qn xs -> DTS.TypeConstructor qn $ map (deleteQuant s) xs
  DTS.TypeOpaque y x -> DTS.TypeOpaque y x
  DTS.TypeUnion x y -> DTS.TypeUnion (deleteQuant s x) (deleteQuant s y)
  DTS.TypeTLString str -> DTS.TypeTLString str
  where

  deleteQuantFunction ta xs r = DTS.TypeFunction
    (foldr S.delete ta (S.intersection ta s))
    (map (map $ deleteQuant s) xs)
    (deleteQuant s r)

type TypeScope = { quantified :: Array DTS.Name, floating :: Array DTS.Name }

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

defineModules :: Map String (String /\ String) -> Array (String /\ Array DTS.Declaration) -> Array (String /\ DTS.Module)
defineModules mm xs = (\(k /\ v) -> k /\ defineModule mm' k v) <$> xs
  where
  mm' = xs <#> fst >>> pursModule # M.fromFoldable # M.union mm

defineModule :: Map String (String /\ String) -> String -> Array DTS.Declaration -> DTS.Module
defineModule mm k xs =
  DTS.Module moduleHead moduleBody
    # cleanModule alias
  where
  moduleHead = (DTS.ModuleHead commentHeader imports)

  alias = M.lookup k mm <#> snd # maybe "Unknown_Alias" identity

  imports = xs
    >>= declToRefs
    <#> (\(DTS.QualName sc _) -> sc)
    # catMaybes
    # A.nub
    <#> (\key -> (key /\ M.lookup key mm) # sequence)
    # catMaybes
    <#> (\(a /\ p /\ _) -> DTS.Import (DTS.Name a) (DTS.Path p))

  commentHeader =
    [ "Auto generated type signatures."
    , "PureScript Module: " <> k
    ]

  moduleBody = (DTS.ModuleBody xs) # resolveModuleBody

declToRefs :: DTS.Declaration -> Array DTS.QualName
declToRefs = case _ of
  DTS.DeclTypeDef _ _ t -> typeToRefs t
  DTS.DeclValueDef _ t -> typeToRefs t
  _ -> []

typeToRefs :: DTS.Type -> Array DTS.QualName
typeToRefs = case _ of
  DTS.TypeNull -> []
  DTS.TypeUndefined -> []
  DTS.TypeString -> []
  DTS.TypeNumber -> []
  DTS.TypeBoolean -> []
  DTS.TypeArray t -> typeToRefs t
  DTS.TypeRecord xs -> xs <#> snd >>= typeToRefs
  DTS.TypeFunction _ xs r -> (xs <#> snd >>= typeToRefs) <> (typeToRefs r)
  DTS.TypeVar _ -> []
  DTS.TypeConstructor qn xs -> [ qn ] <> (xs >>= typeToRefs)
  DTS.TypeOpaque _ _ -> []
  DTS.TypeUnion x y -> typeToRefs x <> typeToRefs y
  DTS.TypeTLString _ -> []
