module PursTsGen
  ( classDef
  , cleanModule
  , constructor
  , defPredicateFn
  , defineModules
  , instanceDef
  , module Exp
  , pursModule
  , typeDef
  , value
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Array as A
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String (Pattern(..), Replacement(..))
import Data.String as St
import Data.Traversable (foldr, sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import PursTsGen.Class.ToTsDef (class GenToTsDefProd, class GenToTsDefSum, class ToTsDef, MyThese(..), genToTsDefProd, genToTsDefSum, genToTsDefSum', genericToTsDef, toTsDef) as Exp
import PursTsGen.Class.ToTsDef (class ToTsDef, toTsDef)
import PursTsGen.Class.ToTsType (class GenRecord, class GenVariant, class ToTsType, genRecord, genVariant, toTsType) as Exp
import PursTsGen.Class.ToTsType (class ToTsPredFn, class ToTsType, Constructor(..), toTsPredFn, toTsType)
import PursTsGen.Lang.TypeScript (defaultVisitor, rewriteModuleTopDown)
import PursTsGen.Lang.TypeScript.DSL (Declaration(..), Import(..), Module(..), ModuleBody(..), ModuleHead(..), Name(..), Path(..), QualName(..), Type(..), emptyLine, lineComment, name, string) as TS
import PursTsGen.Lang.TypeScript.Ops (resolveModuleBody)
import Type.Proxy (Proxy(..))

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
  ] <> toTsDef x

defPredicateFn :: forall a. ToTsPredFn a => String -> Array TS.Type -> a -> TS.Type -> Array TS.Declaration
defPredicateFn n cs x t =
  [ TS.emptyLine
  , TS.DeclValueDef (TS.Name n) $ foldr mkFn init cs
  ]
  where
  mkFn y f = TS.TypeFunction S.empty [ (TS.Name "_") /\ y ] f
  init = toTsPredFn t x

classDef :: forall dummy a. ToTsDef a => String -> dummy -> Proxy a -> Array TS.Declaration
classDef n _ = typeDef n

instanceDef :: String -> TS.Type -> Array TS.Declaration
instanceDef n x =
  [ TS.emptyLine
  , TS.lineComment "Instance"
  , TS.DeclValueDef (TS.Name n) $ x
  ]