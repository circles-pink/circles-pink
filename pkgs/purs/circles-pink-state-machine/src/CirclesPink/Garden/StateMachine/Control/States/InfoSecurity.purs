module CirclesPink.Garden.StateMachine.Control.States.InfoSecurity where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype.Extra ((-#))

infoSecurity
  :: forall m
   . MonadCircles m
  => EnvControl m
  -> { prev :: ActionHandler' m Unit S.UserData ("askEmail" :: S.UserData, "askUsername" :: S.UserData)
     , next :: ActionHandler' m Unit S.UserData ("magicWords" :: S.UserData)
     }
infoSecurity env =
  { prev
  , next
  }
  where
  next set _ _ = do
    pk <- env.generatePrivateKey
    set \st ->
      if isNothing st.privateKey then
        S._magicWords st { privateKey = Just pk, direction = D.Forwards }
      else
        S._magicWords st { direction = D.Forwards }

  prev set _ _ = do
    cfg <- ask
    set \st -> case cfg -# _.extractEmail of
      Left _ -> S._askUsername st { direction = D.Backwards }
      Right _ -> S._askEmail st { direction = D.Backwards }
