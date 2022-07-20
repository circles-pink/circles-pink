module CirclesPink.Garden.StateMachine.Control.States.InfoGeneral where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S

infoGeneral
  :: forall m
   . Monad m
  => EnvControl m
  -> { next :: ActionHandler' m Unit S.UserData ("askUsername" :: S.UserData) }
infoGeneral _ =
  { next: \set _ _ -> set \st -> S._askUsername st { direction = D._forwards }
  }
