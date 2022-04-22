module CirclesPink.Garden.StateMachine.Control.States.InfoGeneral where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Trans.Class (class MonadTrans)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler)

infoGeneral ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { next :: ActionHandler t m Unit S.UserData ( "askUsername" :: S.UserData ) }
infoGeneral _ =
  { next: \set _ _ -> set \st -> S._askUsername st { direction = D._forwards }
  }
