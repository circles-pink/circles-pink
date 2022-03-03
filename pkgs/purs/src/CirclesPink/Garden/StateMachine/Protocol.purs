module CirclesPink.Garden.StateMachine.Protocol
  ( CirclesProtocol
  ) where

import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')

type CirclesProtocol
  = P.Protocol
      ( infoGeneral ::
          P.State
            ( next :: P.Action ("infoGeneral" :> "askUsername" :> Nil')
            )
      , askUsername ::
          P.State
            ( prev :: P.Action ("askUsername" :> "infoGeneral" :> Nil')
            , setUsername :: P.Action ("askUsername" :> Nil')
            , next :: P.Action ("askUsername" :> "askEmail" :> Nil')
            )
      , askEmail ::
          P.State
            ( prev :: P.Action ("askEmail" :> "askUsername" :> Nil')
            , setEmail :: P.Action ("askEmail" :> Nil')
            , setTerms :: P.Action ("askEmail" :> Nil')
            , setPrivacy :: P.Action ("askEmail" :> Nil')
            , next :: P.Action ("askEmail" :> "infoSecurity" :> Nil')
            )
      , infoSecurity ::
          P.State
            ( prev :: P.Action ("infoSecurity" :> "askEmail" :> Nil')
            , next :: P.Action ("infoSecurity" :> "magicWords" :> Nil')
            )
      , magicWords ::
          P.State
            ( prev :: P.Action ("magicWords" :> "infoSecurity" :> Nil')
            -- , next :: P.Action ("...." :> Nil')
            )
      )
