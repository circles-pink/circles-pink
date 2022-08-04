module PursTsGen.Class.ToPursType where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import PursTsGen.Data.ABC (A, B, C, D)
import PursTsGen.Lang.PureScript.Type as PS
import Type.Proxy (Proxy(..))
import Undefined (undefined)

class ToPursType a where
  toPursType :: a -> PS.Type

instance toPursTypeProxy :: ToPursType a => ToPursType (Proxy a) where
  toPursType _ = toPursType (undefined :: a)

instance toPursTypeMaybe :: ToPursType a => ToPursType (Maybe a) where
  toPursType _ = PS.mkType (PS.qualName "Data_Maybe" "Maybe") [ toPursType (Proxy :: _ a) ]

instance toPursTypeTuple :: (ToPursType a, ToPursType b) => ToPursType (Tuple a b) where
  toPursType _ = PS.mkType (PS.qualName "Data_Tuple" "Tuple")
    [ toPursType (Proxy :: _ a)
    , toPursType (Proxy :: _ b)
    ]

instance toPursTypeEither :: (ToPursType a, ToPursType b) => ToPursType (Either a b) where
  toPursType _ = PS.mkType (PS.qualName "Data_Either" "Either")
    [ toPursType (Proxy :: _ a)
    , toPursType (Proxy :: _ b)
    ]

instance toPursTypeA :: ToPursType A where
  toPursType _ = PS.var $ PS.Name "a"

instance toPursTypeB :: ToPursType B where
  toPursType _ = PS.var $ PS.Name "b"

instance toPursTypeC :: ToPursType C where
  toPursType _ = PS.var $ PS.Name "c"

instance toPursTypeD :: ToPursType D where
  toPursType _ = PS.var $ PS.Name "d"

instance toPursTypeVariant :: ToPursType (Variant v) where
  toPursType _ = PS.var $ PS.Name "TODO"

