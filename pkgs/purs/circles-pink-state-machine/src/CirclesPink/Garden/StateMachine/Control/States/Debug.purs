module CirclesPink.Garden.StateMachine.Control.States.Debug where

import Prelude

import CirclesPink.Data.Mnemonic (getMnemonicFromString)
import CirclesPink.Data.PrivateKey (mnemonicToKey)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (runExceptT)
import Data.Maybe (Maybe(..))

debug
  :: forall m
   . Monad m
  => Env.Env m
  -> { coreToWindow :: ActionHandler' m Unit S.DebugState ("debug" :: S.DebugState)
     , setMagicWords :: ActionHandler' m String S.DebugState ("debug" :: S.DebugState)
     }
debug env =
  { coreToWindow
  , setMagicWords: \set _ mw -> set \st -> S._debug st { magicWords = mw }
  }
  where
  coreToWindow _ st _ = do
    let
      maybeMnemonic = getMnemonicFromString st.magicWords
    let
      maybePrivKey = mnemonicToKey <$> maybeMnemonic
    _ <- case maybePrivKey of
      Just pk -> runExceptT $ env.coreToWindow pk
      Nothing -> pure $ pure unit
    pure unit
