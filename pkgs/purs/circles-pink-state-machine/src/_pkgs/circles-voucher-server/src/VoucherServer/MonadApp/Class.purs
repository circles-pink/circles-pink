module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import CirclesCore (TrustAddConnectionOptions)
import CirclesCore as CC
import CirclesPink.Data.Address as C
import Control.Monad.Error.Class (class MonadError)
import Data.Array (replicate)
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Seconds)
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Payload.Client (ClientError)
import Payload.ResponseTypes (Response(..))
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
  , authChallengeDuration :: Seconds
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
  | ErrAuthChallenge

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
  ErrCirclesCore _ -> internalError
  ErrUnknown -> internalError
  ErrBasicAuth -> authError
  ErrGraphQL -> internalError
  ErrGraphQLParse _ -> internalError
  ErrPayloadClient _ -> internalError
  ErrGetVoucherAmount -> internalError
  ErrAuthChallenge -> authError
  where
  authError = Error $ Res.unauthorized $
    StringBody "Authorization failed"

  internalError = Error $ Res.internalError $
    StringBody "Internal server error"

errorToLog :: AppError -> String
errorToLog = case _ of
  ErrCirclesCore e ->
    "Circles Core Error: " <> CC.printErr e
  ErrUnknown ->
    "Unknown error"
  ErrBasicAuth ->
    "Basic Authentication failed"
  ErrGraphQL ->
    "Graph QL Error"
  ErrGraphQLParse msg ->
    "Graph QL Parse Error: " <> msg
  ErrPayloadClient payloadError ->
    "Payload client error"  <> indent 2 (show payloadError)
  ErrGetVoucherAmount ->
    "Failed to get Voucher Amount"
  ErrAuthChallenge ->
    "Challenge Authentication failed"

--------------------------------------------------------------------------------
-- AppEnv
--------------------------------------------------------------------------------

newtype AppEnv m = AppEnv
  { envVars :: AppEnvVars
  , constants :: AppConstants
  , graphNode :: GraphNodeEnv m
  , circlesCore :: CirclesCoreEnv m
  , xbgeClient :: XbgeClientEnv m
  , now :: AE'now m
  }

type CirclesCoreEnv m =
  { getTrusts ::
      CC'getTrusts m
  , getPaymentNote ::
      CC'getPaymentNote m
  , trustAddConnection ::
      CC'trustAddConnection m
  , trustIsTrusted ::
      CC'trustIsTrusted m
  , getSafeAddress ::
      CC'getSafeAddress m
  }

type XbgeClientEnv m =
  { getVoucherProviders ::
      XBG'getVoucherProviders m
  , finalizeVoucherPurchase ::
      XBG'finalizeVoucherPurchase m
  , getVouchers ::
      XBG'getVouchers m
  }

type GraphNodeEnv m =
  { getTransferMeta ::
      GN'getTransferMeta m
  , getTransactions ::
      GN'getTransactions m
  }


type AE'now :: forall k. (Type -> k) -> k
type AE'now m =
  m Instant

type GN'getTransferMeta m =
  TransferId -> m TransferMeta

type GN'getTransactions m =
  { toAddress :: Address } -> m (Array Transfer)

type CC'getTrusts m =
  C.Address -> m (Set C.Address)

type CC'getPaymentNote m =
  String -> m String

type CC'trustAddConnection m =
  TrustAddConnectionOptions -> m String

type CC'trustIsTrusted m =
  { safeAddress :: ChecksumAddress
  , limit :: Int
  }
  -> m CC.TrustIsTrustedResult

type CC'getSafeAddress m =
  Address -> m Address

type XBG'getVoucherProviders m =
  {} -> m (Response { data :: Array VoucherProvider })

type XBG'finalizeVoucherPurchase m =
  { body ::
      { safeAddress :: Address
      , providerId :: VoucherProviderId
      , amount :: VoucherAmount
      , transactionId :: TransferId
      }
  }
  -> m (Response { data :: VoucherEncrypted })

type XBG'getVouchers m =
  { query ::
      { safeAddress :: Maybe Address }
  }
  -> m (Response { data :: Array VoucherEncrypted })

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

indent :: Int -> String -> String
indent n = split (Pattern "\n")
  >>> map (\line -> (fromCharArray $ replicate n ' ') <> line)
  >>> joinWith "\n"

getResponseData :: forall r a. Response { data :: a | r } -> a
getResponseData = un Response >>> _.body >>> _.data
