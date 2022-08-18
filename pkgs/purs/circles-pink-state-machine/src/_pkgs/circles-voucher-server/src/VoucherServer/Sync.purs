module VoucherServer.Sync where

import Prelude

import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.Reader (ask)
import Debug.Extra (todo)
import Effect.Aff (Aff)
import VoucherServer.MonadApp (class MonadApp, AppEnv(..), AppProdM, errorToLog, runAppProdM)
import VoucherServer.Spec.Types (VoucherEncrypted)
import VoucherServer.Types (Transfer(..))

finalizeTx' :: AppEnv AppProdM -> Transfer -> ExceptT String Aff VoucherEncrypted
finalizeTx' appEnv trans = finalizeTx trans
  # runAppProdM appEnv
  # ExceptT
  # withExceptT errorToLog

finalizeTx :: forall m. MonadApp m => Transfer -> m VoucherEncrypted
finalizeTx (Transfer { from, amount, id }) = do
  AppEnv { graphNode } <- ask
  -- transferMeta <- graphNode.getTransferMeta id
  pure todo