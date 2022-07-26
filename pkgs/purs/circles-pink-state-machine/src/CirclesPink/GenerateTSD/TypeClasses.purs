module CirclesPink.GenerateTSD.TypeClasses where

import Prelude

import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.GenerateTSD.Class (class ToTsType, toTsType)
import Data.Typelevel.Undefined (undefined)
import Language.TypeScript.DTS as DTS
import Type.Proxy (Proxy(..))

data ORD = ORD

instance eqORD :: Eq ORD where
  eq = undefined

instance ordORD :: Ord ORD where
  compare = undefined

instance toTsType :: ToTsType ORD where
  toTsType _ = DTS.TypeVar $ DTS.Name "ORD"
  
