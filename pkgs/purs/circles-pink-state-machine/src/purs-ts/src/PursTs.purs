module Pursts
  ( cleanModule
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Language.TypeScript.DTS as DTS

class Clean a where
  clean :: String -> a -> a

instance cleanModule' :: Clean DTS.Module where
  clean m (DTS.Module xs) = DTS.Module $ clean m <$> xs

instance cleanDeclaration :: Clean DTS.Declaration where
  clean m x = case x of
    DTS.DeclTypeDef x' y t -> DTS.DeclTypeDef x' y $ clean m t
    DTS.DeclValueDef x' t -> DTS.DeclValueDef x' $ clean m t
    _ -> x

instance cleanType :: Clean DTS.Type where
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

cleanModule :: String -> DTS.Module -> DTS.Module
cleanModule = clean