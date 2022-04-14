module CirclesPink.Garden.StateMachineV2.Onboarding where

import Prelude
import StadiumV2 as S
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State (UsernameApiResult, EmailApiResult)
import StadiumV2 (type (<:), type ($), type (</>), type (#))
import Wallet.PrivateKey (PrivateKey)

type UserData
  = { direction :: D.Direction
    , username :: String
    , usernameApiResult :: UsernameApiResult
    , email :: String
    , emailApiResult :: EmailApiResult
    , terms :: Boolean
    , privacy :: Boolean
    , privateKey :: PrivateKey
    }

type StateMachine
  = S.StateMachine_
      ( infoGeneral ::
          S.State_
            ( next :: S.Action_ # S.Target (S.Rel "askUsername")
            )
      , askUsername ::
          S.State {}
            ( prev :: S.Action_ # S.Target (S.Rel "infoGeneral")
            , setUsername :: S.Action String # S.Target S.Self
            , next :: S.Action_ # S.Target S.Self # S.Target (S.Rel "askEmail")
            )
      )
      ()
