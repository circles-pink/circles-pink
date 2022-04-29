module CirclesPink.Garden.StateMachine.Control.States.Landing where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Trans.Class (class MonadTrans)

landing ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { signUp :: ActionHandler t m Unit S.LandingState ( "infoGeneral" :: S.UserData )
  , signIn :: ActionHandler t m Unit S.LandingState ( "login" :: S.LoginState )
  }
landing _ =
  { signUp: \set _ _ -> set \_ -> S.init
  , signIn: \set _ _ -> set \_ -> S.initLogin
  }
