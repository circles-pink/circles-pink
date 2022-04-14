module CirclesPink.Garden.StateMachineV2 where

import CirclesPink.Garden.StateMachineV2.Onboarding as Onboarding
import StadiumV2 (type (#), type (</>))
import StadiumV2 as S

type StateMachine
  = S.StateMachine {}
      ( landing ::
          S.State {}
            ( signUp :: S.Action_ # S.Target (S.Rel "onboarding" </> "infoGeneral")
            , signIn :: S.Action_ # S.Target (S.Rel "login")
            )
      )
      ( onboarding :: Onboarding.StateMachine
      )
