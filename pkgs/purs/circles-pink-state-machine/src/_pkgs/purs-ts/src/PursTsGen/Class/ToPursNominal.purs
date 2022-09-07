module PursTsGen.Class.ToPursNominal where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Typelevel.Undefined (undefined)
import Type.Proxy (Proxy)

data PursNominal = PursNominal String String

class ToPursNominal a where
  toPursNominal :: a -> PursNominal

--------------------------------------------------------------------------------

instance ToPursNominal (Maybe a) where
  toPursNominal _ = PursNominal "Data.Maybe" "Maybe"

instance ToPursNominal (Either a b) where
  toPursNominal _ = PursNominal "Data.Either" "Either"

instance ToPursNominal (Tuple a b) where
  toPursNominal _ = PursNominal "Data.Tuple" "Tuple"

instance ToPursNominal a => ToPursNominal (Proxy a) where
  toPursNominal _ = toPursNominal (undefined :: a)

