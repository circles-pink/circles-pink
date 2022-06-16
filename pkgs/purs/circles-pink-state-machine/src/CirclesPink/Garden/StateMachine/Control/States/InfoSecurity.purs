module CirclesPink.Garden.StateMachine.Control.States.InfoSecurity where

import Prelude

import CirclesPink.Garden.StateMachine.Config (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype.Extra ((-|))

infoSecurity
  :: forall m
   . Monad m
  => Env.Env m
  -> CirclesConfig m
  -> { prev :: ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData, "askUsername" :: S.UserData)
     , next :: ActionHandler' m Unit S.UserData ("magicWords" :: S.UserData)
     }
infoSecurity env cfg =
  { prev
  , next
  }
  where
  next set _ _ = do
    pk <- env.generatePrivateKey
    set \st ->
      if isNothing st.privateKey then
        S._magicWords st { privateKey = Just pk, direction = D._forwards }
      else
        S._magicWords st { direction = D._forwards }

  prev set _ _ = set \st -> case cfg -| _.extractEmail of
    Nothing -> S._askUsername st { direction = D._backwards }
    Just _ -> S._askEmail st { direction = D._backwards }
