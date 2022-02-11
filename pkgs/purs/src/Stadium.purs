module Stadium
  ( Control
  , class GetState
  , class GetState'
  , class GetTypes
  , mkController
  ) where

import Prelude
import Core.State.Onboard (State(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import StateMachine.Protocol (AskUserName, InfoGeneral, AskEmail)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

class GetState :: forall k1 k2. k1 -> k2 -> Constraint
class GetState ptc st | ptc -> st

-- instance getState ::
--   GetState ptc ( Variant
--         ( infoGeneral :: InfoGeneral
--         , askUserName :: AskUserName
--         , askEmail :: AskEmail
--         )
--     )
instance getState :: (RowToList ptc rl, GetState' rl r) => GetState (Record ptc) (Variant r)

class GetState' :: RowList Type -> Row Type -> Constraint
class GetState' rl r | rl -> r

instance getState'Nil :: GetState' Nil ()

instance getState'Cons ::
  ( GetState' tail r'
  , Cons s d r' r
  , Cons "data" d trash t
  ) =>
  GetState' (Cons s (Record t) tail) r

class GetAction ptc ac

class GetTypes ptc st ac | ptc -> st, ptc -> ac

instance getTypes :: GetTypes Unit Int String

-- instance getState :: GetState 
type Control st ac m
  = (st -> m Unit) -> ac -> st -> m Unit

mkController ::
  forall ptc ac st m.
  GetState ptc st => Proxy ptc -> Control st ac m -> Control st ac m
mkController _ ctr = ctr
