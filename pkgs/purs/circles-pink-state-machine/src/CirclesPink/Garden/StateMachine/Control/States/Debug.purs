module CirclesPink.Garden.StateMachine.Control.States.Debug where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (runExceptT)
import Wallet.PrivateKey as P

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
      mnemonic = P.getMnemonicFromString st.magicWords
    let
      privKey = P.mnemonicToKey mnemonic
    _ <- runExceptT $ env.coreToWindow privKey
    pure unit
