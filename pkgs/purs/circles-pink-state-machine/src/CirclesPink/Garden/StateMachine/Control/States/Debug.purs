module CirclesPink.Garden.StateMachine.Control.States.Debug where

import Prelude

import CirclesPink.Data.Mnemonic (getMnemonicFromString)
import CirclesPink.Data.PrivateKey (mnemonicToKey)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (lift, runExceptT)
import Data.Either (Either(..), note)

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
      eitherMnemonic = note ("Cannot get Mnemonic: " <> "'" <> st.magicWords <> "'") $ getMnemonicFromString st.magicWords
    let
      eitherPrivKey = mnemonicToKey <$> eitherMnemonic

    _ <- runExceptT case eitherPrivKey of
      Left err -> lift $ env.logInfo err
      Right pk -> env.coreToWindow pk

    pure unit
