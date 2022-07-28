module CirclesPink.GenerateTSD.TypeClasses where

import Prelude

import CirclesPink.GenerateTSD.Class (class ToTsDef, class ToTsType)
import Data.ABC (A)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import Language.TypeScript.DTS as DTS


data ClassOrd :: forall k. k -> Type
data ClassOrd a

instance toTsTypeDefClassOrd :: ToTsDef (ClassOrd A) where
  toTsDef _ = DTS.TypeOpaque (DTS.QualName (Just "Data_Ord") "Ord") $ DTS.Name <$> [ "A" ]

data ORD = ORD


instance eqORD :: Eq ORD where
  eq = undefined

instance ordORD :: Ord ORD where
  compare = undefined

instance toTsTypeORD :: ToTsType ORD where
  toTsType _ = DTS.TypeVar $ DTS.Name "ORD"



