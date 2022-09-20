module CirclesPink.GenerateTSD.TypeClasses where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import PursTsGen (class ToTsDef, class ToTsType)
import PursTsGen.Data.ABC (A)
import PursTsGen.Lang.TypeScript.DSL as TS

data ClassOrd :: forall k. k -> Type
data ClassOrd a

instance ToTsDef (ClassOrd A) where
  toTsDef _ =
    pure $ TS.typeDef (TS.name "Ord") []
      $ TS.TypeOpaque (TS.QualName (Just "Data_Ord") "Ord")
      $ TS.Name <$> [ "A" ]

data ORD = ORD

instance Eq ORD where
  eq = undefined

instance Ord ORD where
  compare = undefined

instance ToTsType ORD where
  toTsType _ = TS.TypeVar $ TS.Name "ORD"
--  toPursType _ = PT.var $ PT.name "ord"

