module Stadium
  ( Control
  , class GetState
  , class GetState'
  , class GetTypes
  , mkController
  , class GetCtl
  , mkController'
  ) where

import Prelude
import Data.Variant (Variant)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy)
import Undefined (undefined)

class GetState :: forall k1 k2. k1 -> k2 -> Constraint
class GetState ptc st | ptc -> st

instance getState :: (RowToList ptc rl, GetState' rl r) => GetState (Record ptc) (Variant r)

class GetState' :: RowList Type -> Row Type -> Constraint
class GetState' rl r | rl -> r

instance getState'Nil :: GetState' Nil ()

instance getState'Cons ::
  ( GetState' tail r'
  , Cons s d r' r
  ) =>
  GetState' (Cons s (Record t) tail) r

class GetAction :: forall k1 k2. k1 -> k2 -> Constraint
class GetAction ptc ac

class GetTypes :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class GetTypes ptc st ac | ptc -> st, ptc -> ac

instance getTypes :: GetTypes Unit Int String

-- instance getState :: GetState 
type Control st ac m
  = (st -> m Unit) -> ac -> st -> m Unit

class GetCtl st ac ptc ctr | st -> ctr

instance getCtl ::
  GetCtl st ac ptc ( infoGeneral :: { next :: (st -> m unit) -> m unit }
    , askUserName :: { next :: (st -> m unit) -> m unit }
    )

mkController ::
  forall ptc ac st m.
  GetState ptc st => Proxy ptc -> Control st ac m -> Control st ac m
mkController _ ctr = ctr

mkController' ::
  forall ptc ac st m ctr.
  GetState ptc st => GetCtl st ac ptc ctr => Proxy ptc -> Record ctr -> Control st ac m
mkController' _ ctr = undefined
