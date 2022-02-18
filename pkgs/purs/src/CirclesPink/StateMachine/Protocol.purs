module CirclesPink.StateMachine.Protocol
  ( CirclesProtocol
  ) where

import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')

type CirclesProtocol
  = P.Protocol
      ( state1 ::
          P.State
            ( action1 :: P.Action Nil'
            , action2 :: P.Action ("state2" :> "state1" :> Nil')
            )
      , state2 :: P.State ()
      )
