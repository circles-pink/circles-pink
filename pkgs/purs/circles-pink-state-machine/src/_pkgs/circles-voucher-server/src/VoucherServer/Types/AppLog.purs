module VoucherServer.Types.AppLog where

import VoucherServer.Prelude

import Data.Array (replicate)
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits (fromCharArray)
import VoucherServer.Spec.Types (VoucherEncrypted)
import VoucherServer.Types (Transfer)
import VoucherServer.Types.AppError (AppError, errorToLog)


data AppLog
  = LogSyncStart
  | LogSyncEnd
  | LogSyncFetchedTxs Int
  | LogStartFinalizeTx Transfer
  | LogFinishFinalizeTx VoucherEncrypted
  | LogCatchedFinalizeTxError AppError
  | LogRedeem

logToString :: AppLog -> String
logToString = case _ of
  LogSyncStart -> "Tiggered transaction sync."
  LogSyncEnd -> "Finished transaction sync."
  LogSyncFetchedTxs n -> "Fetched " <> show n <> " transactions."
  LogStartFinalizeTx _ -> "Start to finalize Transaction."
  LogFinishFinalizeTx ve -> "Finish finalizing Transaction. " <> show ve
  LogCatchedFinalizeTxError err -> joinWith "\n"
    [ "Catched error while finalizing transaction. Skipping!"
    , indent 2 $ errorToLog err
    ]
  LogRedeem -> "In the future we'll pay back the amount..."

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n = split (Pattern "\n")
  >>> map (\line -> (fromCharArray $ replicate n ' ') <> line)
  >>> joinWith "\n"
