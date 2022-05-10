module CirclesPink.Garden.StateMachine.Control.States.InfoSecurity where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Wallet.PrivateKey as P

infoSecurity ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { prev :: ActionHandler t m Unit S.UserData ( "askEmail" :: S.UserData )
  , next :: ActionHandler t m Unit S.UserData ( "magicWords" :: S.UserData )
  }
infoSecurity env =
  { prev: \set _ _ -> set \st -> S._askEmail st { direction = D._backwards }
  , next
  }
  where
  next set _ _ = do
    pk <- lift env.generatePrivateKey
    set \st ->
      if P.sampleKey == st.privateKey then
        S._magicWords st { privateKey = pk, direction = D._forwards }
      else
        S._magicWords st { direction = D._forwards }
