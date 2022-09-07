module CirclesPink.Garden.StateMachine.Direction where

import Prelude

import PursTsGen (class ToPursType, class ToTsDef, class ToTsType, defaultToPursType', defaultToTsDef', defaultToTsType')
import PursTsGen.Class.ToPursNominal (class ToPursNominal, PursNominal(..))

data Direction = Forwards | Backwards

unDirection :: forall z. { onForwards :: Unit -> z, onBackwards :: Unit -> z } -> Direction -> z
unDirection { onForwards, onBackwards } d = case d of
  Forwards -> onForwards unit
  Backwards -> onBackwards unit

--------------------------------------------------------------------------------

instance ToPursNominal Direction where
  toPursNominal _ = PursNominal "CirclesPink.Garden.StateMachine.Direction" "Direction"

instance ToTsType Direction where
  toTsType = defaultToTsType' []

instance ToTsDef Direction where
  toTsDef = defaultToTsDef' []

instance ToPursType Direction where
  toPursType = defaultToPursType' []
