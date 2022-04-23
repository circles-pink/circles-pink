module CirclesPink.Garden.StateMachine.Control.States.Debug where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Wallet.PrivateKey as P

debug ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { coreToWindow :: ActionHandler t m Unit S.DebugState ( "debug" :: S.DebugState )
  , setMagicWords :: ActionHandler t m String S.DebugState ( "debug" :: S.DebugState )
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
    _ <- run $ env.coreToWindow privKey
    pure unit
