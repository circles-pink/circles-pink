module CirclesPink.Garden.StateMachine.Protocol
  ( CirclesProtocol
  ) where

import Stadium.Type.Protocol as P
import Type.Data.List (type (:>), Nil')

type CirclesProtocol
  = P.Protocol
      ( landing ::
          P.State
            ( signUp :: P.Action ("infoGeneral" :> Nil')
            , signIn :: P.Action ("login" :> Nil')
            , checkForSession :: P.Action ("landing" :> "trusts" :> "dashboard" :> Nil')
            )
      , infoGeneral ::
          P.State
            ( next :: P.Action ("askUsername" :> Nil')
            )
      , askUsername ::
          P.State
            ( prev :: P.Action ("infoGeneral" :> Nil')
            , setUsername :: P.Action ("askUsername" :> Nil')
            , next :: P.Action ("askUsername" :> "askEmail" :> Nil')
            )
      , askEmail ::
          P.State
            ( prev :: P.Action ("askUsername" :> Nil')
            , setEmail :: P.Action ("askEmail" :> Nil')
            , setTerms :: P.Action ("askEmail" :> Nil')
            , setPrivacy :: P.Action ("askEmail" :> Nil')
            , next :: P.Action ("askEmail" :> "infoSecurity" :> Nil')
            )
      , infoSecurity ::
          P.State
            ( prev :: P.Action ("askEmail" :> Nil')
            , next :: P.Action ("magicWords" :> Nil')
            )
      , magicWords ::
          P.State
            ( prev :: P.Action ("infoSecurity" :> Nil')
            , newPrivKey :: P.Action ("magicWords" :> Nil')
            , next :: P.Action ("submit" :> Nil')
            )
      , submit ::
          P.State
            ( prev :: P.Action ("magicWords" :> Nil')
            , submit :: P.Action ("submit" :> "trusts" :> Nil')
            )
      , dashboard ::
          P.State
            ( logout :: P.Action ("landing" :> Nil')
            , getTrusts :: P.Action ("dashboard" :> Nil')
            , addTrustConnection :: P.Action ("dashboard" :> Nil')
            , removeTrustConnection :: P.Action ("dashboard" :> Nil')
            , getBalance :: P.Action ("dashboard" :> Nil')
            , getUsers :: P.Action ("dashboard" :> Nil')
            , transfer :: P.Action ("dashboard" :> Nil')
            , userSearch :: P.Action ("dashboard" :> Nil')
            )
      , login ::
          P.State
            ( login :: P.Action ("login" :> "dashboard" :> "trusts" :> Nil')
            , signUp :: P.Action ("infoGeneral" :> Nil')
            , setMagicWords :: P.Action ("login" :> Nil')
            )
      , trusts ::
          P.State
            ( getSafeStatus :: P.Action ("trusts" :> Nil')
            , finalizeRegisterUser :: P.Action ("dashboard" :> "trusts" :> Nil')
            )
      , debug ::
          P.State
            ( coreToWindow :: P.Action ("debug" :> Nil')
            , setMagicWords :: P.Action ("debug" :> Nil')
            )
      )
