module CirclesPink.StateMachine
  ( CirclesStateMachine
  , check
  , _circlesStateMachine
  ) where

import Prelude
import CirclesPink.StateMachine.Action as A
import CirclesPink.StateMachine.Protocol as P
import CirclesPink.StateMachine.State as S
import Stadium.Type.StateMachine as STM
import Type.Proxy (Proxy(..))

type CirclesStateMachine
  = STM.StateMachine
      P.CirclesProtocol
      S.CirclesState
      A.CirclesAction

_circlesStateMachine :: Proxy CirclesStateMachine
_circlesStateMachine = Proxy :: _ CirclesStateMachine

check :: Unit
check = STM.validate (Proxy :: _ CirclesStateMachine)
