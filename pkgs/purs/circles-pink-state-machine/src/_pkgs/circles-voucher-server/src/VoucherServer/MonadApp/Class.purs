module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import CirclesCore (TrustAddConnectionOptions)
import CirclesCore as CC
import CirclesPink.Data.Address as C
import Control.Monad.Error.Class (class MonadError)
import Data.Array (replicate)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits (fromCharArray)
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Payload.Client (ClientError)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.Spec.Types (TransferId, VoucherAmount, VoucherEncrypted, VoucherProvider, VoucherProviderId)
import VoucherServer.Specs.Xbge (Address)
import VoucherServer.Types (Transfer, TransferMeta)

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class
  ( MonadError AppError m
  , MonadAsk (AppEnv m) m
  ) <=
  MonadApp m where
  log :: AppLog -> m Unit

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

type AppConstants =
  { trustLimitPercentage :: Number
  }

--------------------------------------------------------------------------------
-- Log
--------------------------------------------------------------------------------

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
-- Error
--------------------------------------------------------------------------------

data AppError
  = ErrCirclesCore CCErrAll
  | ErrUnknown
  | ErrBasicAuth
  | ErrGraphQL
  | ErrGraphQLParse String
  | ErrPayloadClient ClientError
  | ErrGetVoucherAmount

derive instance genericVSE :: Generic AppError _
derive instance eqVSE :: Eq AppError
instance showVSE :: Show AppError where
  show = genericShow

-- Due to a compiler bug (unknown module: Partially applied type synonyms)
-- we have to redefine this type inside the current module
type CCErrAll = Variant
  ( CC.ErrParseAddress
      + CC.ErrNative
      + CC.ErrService
      + CC.ErrInvalidUrl
      + CC.ErrApi
      + CC.ErrNotGivenOrAllowed
      + CC.ErrNullReturn
      + ()
  )

errorToFailure :: AppError -> Failure
errorToFailure = case _ of
  ErrCirclesCore _ -> internalServerError
  ErrUnknown -> internalServerError
  ErrBasicAuth -> Error $ Res.internalError $ StringBody "Authorization failed"
  ErrGraphQL -> internalServerError
  ErrGraphQLParse _ -> internalServerError
  ErrPayloadClient _ -> internalServerError
  ErrGetVoucherAmount -> internalServerError
  where
  internalServerError = Error $ Res.internalError $ StringBody "Internal server error"

errorToLog :: AppError -> String
errorToLog = case _ of
  ErrCirclesCore e -> "Circles Core Error: " <> CC.printErr e
  ErrUnknown -> "Unknown error"
  ErrBasicAuth -> "Basic Authentication failed"
  ErrGraphQL -> "Graph QL Error"
  ErrGraphQLParse msg -> "Graph QL Parse Error: " <> msg
  ErrPayloadClient payloadError -> "Payload client error:" <> indent 2 (show payloadError)
  ErrGetVoucherAmount -> "Failed to get Voucher Amount"

--------------------------------------------------------------------------------
-- AppEnv
--------------------------------------------------------------------------------

newtype AppEnv m = AppEnv
  { envVars :: AppEnvVars
  , constants :: AppConstants
  , graphNode :: GraphNodeEnv m
  , circlesCore :: CirclesCoreEnv m
  , xbgeClient :: XbgeClientEnv m
  }

--------------------------------------------------------------------------------
-- GraphNodeEnv
--------------------------------------------------------------------------------

type GraphNodeEnv m =
  { getTransferMeta :: GraphNodeEnv'getTransferMeta m
  , getTransactions :: GraphNodeEnv'getTransactions m
  }

type GraphNodeEnv'getTransferMeta m = TransferId -> m TransferMeta

type GraphNodeEnv'getTransactions m = { toAddress :: Address } -> m (Array Transfer)

--------------------------------------------------------------------------------
-- CirclesCoreEnv
--------------------------------------------------------------------------------
type CirclesCoreEnv m =
  { getTrusts ::
      CirclesCoreEnv'getTrusts m
  , getPaymentNote ::
      CirclesCoreEnv'getPaymentNote m
  , trustAddConnection ::
      CirclesCoreEnv'trustAddConnection m
  , trustIsTrusted ::
      CirclesCoreEnv'trustIsTrusted m
  }

type CirclesCoreEnv'getTrusts m = C.Address -> m (Set C.Address)

type CirclesCoreEnv'getPaymentNote m = String -> m String

type CirclesCoreEnv'trustAddConnection m = TrustAddConnectionOptions -> m String

type CirclesCoreEnv'trustIsTrusted m =
  { safeAddress :: ChecksumAddress
  , limit :: Int
  }
  -> m CC.TrustIsTrustedResult

--------------------------------------------------------------------------------
-- XbgeClientEnv
--------------------------------------------------------------------------------
type XbgeClientEnv m =
  { getVoucherProviders :: XbgeClientEnv'getVoucherProviders m
  , finalizeVoucherPurchase :: XbgeClientEnv'finalizeVoucherPurchase m
  , getVouchers :: XbgeClientEnv'getVouchers m
  }

type XbgeClientEnv'getVoucherProviders m =
  {}
  -> m (Response { data :: Array VoucherProvider })

type XbgeClientEnv'finalizeVoucherPurchase m =
  { body ::
      { safeAddress :: Address
      , providerId :: VoucherProviderId
      , amount :: VoucherAmount
      , transactionId :: TransferId
      }
  }
  -> m (Response { data :: VoucherEncrypted })

type XbgeClientEnv'getVouchers m =
  { query :: { safeAddress :: Maybe Address } }
  -> m (Response { data :: Array VoucherEncrypted })

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n = split (Pattern "\n")
  >>> map (\line -> (fromCharArray $ replicate n ' ') <> line)
  >>> joinWith "\n"