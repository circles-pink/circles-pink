module Stadium.Reflect
  ( SmAction
  , SmState
  , StateMachine
  , reflectStateMachine
  , reflectStateMachine'
  , class ReflectStateMachine
  , class ReflectSmState
  , reflectSmState
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Stadium.Signature (class Signature, showSignature)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

--------------------------------------------------------------------------------
type SmAction
  = { data :: String
    , to :: Array String
    }

--------------------------------------------------------------------------------
type SmActions
  = Array (String /\ SmAction)

class ReflectSmActions :: RowList Type -> Constraint
class ReflectSmActions rl where
  reflectSmActions :: Proxy rl -> SmActions

instance reflectSmActionsNil :: ReflectSmActions Nil where
  reflectSmActions _ = []

instance reflectSmActionsCons ::
  (IsSymbol s, ReflectSmState r, ReflectSmActions tail) =>
  ReflectSmActions (Cons s (Record r) tail) where
  reflectSmActions _ =
    [ reflectSymbol (Proxy :: _ s) /\ undefined ]
      <> reflectSmActions (Proxy :: _ tail)

--------------------------------------------------------------------------------
type StateMachine
  = Array (String /\ SmState)

class ReflectStateMachine :: RowList Type -> Constraint
class ReflectStateMachine rl where
  reflectStateMachine' :: Proxy rl -> StateMachine

instance reflectStateMachineNil :: ReflectStateMachine Nil where
  reflectStateMachine' _ = []

instance reflectStateMachineCons ::
  (IsSymbol s, ReflectSmState r, ReflectStateMachine tail) =>
  ReflectStateMachine (Cons s (Record r) tail) where
  reflectStateMachine' _ =
    [ reflectSymbol (Proxy :: _ s) /\ reflectSmState (Proxy :: _ r) ]
      <> reflectStateMachine' (Proxy :: _ tail)

reflectStateMachine :: forall r rl. RowToList r rl => ReflectStateMachine rl => Proxy (Record r) -> StateMachine
reflectStateMachine _ = reflectStateMachine' (Proxy :: _ rl)

--------------------------------------------------------------------------------
type SmState
  = { data :: String
    , actions :: Array (String /\ SmAction)
    }

class ReflectSmState :: Row Type -> Constraint
class ReflectSmState r where
  reflectSmState :: Proxy r -> SmState

instance reflectSmStateInst :: (Signature d, Cons "data" d trash r) => ReflectSmState r where
  reflectSmState _ =
    { data: showSignature (Proxy :: _ d)
    , actions: []
    }
