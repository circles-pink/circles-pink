module CirclesPink.StateMachine
  ( CirclesStateMachine
  ) where

import CirclesPink.StateMachine.Action as A
import CirclesPink.StateMachine.Protocol as P
import CirclesPink.StateMachine.State as S
import Stadium.Type.StateMachine as STM

type CirclesStateMachine
  = STM.StateMachine
      P.CirclesProtocol
      S.CirclesState
      A.CirclesAction
