module CirclesPink.StateMachine.Protocol
  ( CirclesProtocol
  ) where

import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')

type CirclesProtocol
  = P.Protocol
      ( infoGeneral ::
          P.State
            ( next :: P.Action ("askUsername" :> Nil')
            )
      , askUsername ::
          P.State
            ( prev :: P.Action ("infoGeneral" :> Nil')
            , setUsername :: P.Action ("askUsername" :> Nil')
            , next :: P.Action ("askEmail" :> Nil')
            )
      , askEmail ::
          P.State
            ( prev :: P.Action ("askUsername" :> Nil')
            , setEmail :: P.Action ("askEmail" :> Nil')
            )
      )
