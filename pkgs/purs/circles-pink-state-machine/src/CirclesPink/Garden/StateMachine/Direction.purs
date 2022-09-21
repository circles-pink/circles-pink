module CirclesPink.Garden.StateMachine.Direction where

import CirclesPink.Prelude

data Direction = Forwards | Backwards

unDirection :: forall z. { onForwards :: Unit -> z, onBackwards :: Unit -> z } -> Direction -> z
unDirection { onForwards, onBackwards } d = case d of
  Forwards -> onForwards unit
  Backwards -> onBackwards unit

--------------------------------------------------------------------------------

instance ToPursNominal Direction where
  toPursNominal _ = PursNominal "CirclesPink.Garden.StateMachine.Direction" "Direction"

instance ToTsType Direction where
  toTsType = typeRefToTsType' []

instance ToTsDef Direction where
  toTsDef = opaqueToTsDef' []

instance ToPursType Direction where
  toPursType = defaultToPursType' []
