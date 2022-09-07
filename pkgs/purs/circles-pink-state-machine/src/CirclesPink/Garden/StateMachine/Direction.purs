module CirclesPink.Garden.StateMachine.Direction
  ( Direction(..)
  ) where


import PursTsGen (class ToPursType, class ToTsDef, class ToTsType, PursType(..), defaultToPursType, defaultToTsDef, defaultToTsType)

data Direction = Forwards | Backwards

--------------------------------------------------------------------------------

ptDirection :: PursType
ptDirection = PursType "CirclesPink.Garden.StateMachine.Direction" "Direction"

instance ToTsType Direction where
  toTsType _ = defaultToTsType ptDirection []

instance ToTsDef Direction where
  toTsDef _ = defaultToTsDef ptDirection []

instance ToPursType Direction where
  toPursType _ = defaultToPursType ptDirection []
