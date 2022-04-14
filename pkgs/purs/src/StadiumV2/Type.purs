module StadiumV2.Type where

import Prelude
import Type.Data.List (List')

data StateMachine'

data State'

data Action'

data Path'

foreign import data StateMachine :: Type -> Row State' -> Row StateMachine' -> StateMachine'

class CStateMachineState :: Type -> StateMachine' -> Constraint
class CStateMachineState a stm

instance c :: CStateMachineState a (StateMachine a b c)

foreign import data State :: Type -> Row Action' -> State'

foreign import data Action :: Type -> Action'

foreign import data Rel :: Symbol -> Path'

foreign import data Abs :: Symbol -> Path'

foreign import data Self :: Path'

type Action_
  = Action Unit

type State_
  = State Unit

type StateMachine_
  = StateMachine Unit

foreign import data AddSegment :: Path' -> Symbol -> Path'

infixl 1 type AddSegment as </>

foreign import data Target :: Path' -> Action' -> Action'
