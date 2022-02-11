module Stadium.Reflect
  ( SmAction
  , SmState
  , StateMachine
  , ToStates
  , reflectStateMachine'
  , class ReflectStateMachine
  , reflectStateMachine
  , class ReflectSmState
  , reflectSmState
  , class ReflectSmActions
  , reflectSmActions
  , class ReflectSmAction
  , reflectSmAction
  , class ReflectToStates
  , reflectToStates
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

type ToStates
  = Array String

class ReflectToStates :: Type -> Constraint
class ReflectToStates a where
  reflectToStates :: Proxy a -> ToStates

instance reflectToStatesUnit :: ReflectToStates Unit where
  reflectToStates _ = []

instance reflectToStatesTuple :: (IsSymbol s, ReflectToStates tail) => ReflectToStates (Proxy s /\ tail) where
  reflectToStates _ =
    [ reflectSymbol (Proxy :: _ s) ]
      <> reflectToStates (Proxy :: _ tail)

--------------------------------------------------------------------------------
type SmAction
  = { data :: String
    , toStates :: ToStates
    }

class ReflectSmAction :: Row Type -> Constraint
class ReflectSmAction r where
  reflectSmAction :: Proxy r -> SmAction

instance reflectSmActionInst ::
  ( Signature d
  , Cons "data" d trash1 r
  , ReflectToStates t
  , Cons "toStates" t trash2 r
  ) =>
  ReflectSmAction r where
  reflectSmAction _ =
    { data: showSignature (Proxy :: _ d)
    , toStates: reflectToStates (Proxy :: _ t)
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
  (IsSymbol s, ReflectSmAction r, ReflectSmActions tail) =>
  ReflectSmActions (Cons s (Record r) tail) where
  reflectSmActions _ =
    [ reflectSymbol (Proxy :: _ s) /\ reflectSmAction (Proxy :: _ r) ]
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

instance reflectSmStateInst ::
  ( Signature d
  , Cons "data" d trash1 r
  , ReflectSmActions as
  , Cons "actions" (Record a) trash2 r
  , RowToList a as
  ) =>
  ReflectSmState r where
  reflectSmState _ =
    { data: showSignature (Proxy :: _ d)
    , actions: reflectSmActions (Proxy :: _ as)
    }
