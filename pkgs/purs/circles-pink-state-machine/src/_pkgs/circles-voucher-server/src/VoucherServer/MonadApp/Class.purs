module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import CirclesCore (TrustAddConnectionOptions)
import CirclesCore as CC
import CirclesPink.Data.Address as C
import Control.Monad.Error.Class (class MonadError)
import Data.Lens (Lens', lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Payload.Client (ClientError)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import Type.Proxy (Proxy(..))
import VoucherServer.EnvVars (AppEnvVars(..), AppEnvVars')
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
  = LogSync
  | LogStartFinalizeTx Transfer
  | LogFinishFinalizeTx VoucherEncrypted
  | LogRedeem

logToString :: AppLog -> String
logToString = case _ of
  LogSync -> "Tiggered transaction sync."
  LogStartFinalizeTx _ -> "Start to finalize Transaction."
  LogFinishFinalizeTx _ -> "Finish finalizing Transaction."
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
  where
  internalServerError = Error $ Res.internalError $ StringBody "Internal server error"

errorToLog :: AppError -> String
errorToLog = case _ of
  ErrCirclesCore e -> "Circles Core Error: " <> CC.printErr e
  ErrUnknown -> "Unknown error"
  ErrBasicAuth -> "Basic Authentication failed"
  ErrGraphQL -> "Graph QL Error"
  ErrGraphQLParse msg -> "Graph QL Parse Error: " <> msg
  ErrPayloadClient _ -> "Payload client error"

--------------------------------------------------------------------------------
-- AppEnv
--------------------------------------------------------------------------------

newtype AppEnv m = AppEnv (AppEnv' m)
type AppEnv' m =
  { envVars :: AppEnvVars
  , constants :: AppConstants
  , graphNode :: GraphNodeEnv m
  , circlesCore :: CirclesCoreEnv m
  , xbgeClient :: XbgeClientEnv m
  }

--------------------------------------------------------------------------------
-- GraphNodeEnv
--------------------------------------------------------------------------------

newtype GraphNodeEnv m = GraphNodeEnv (GraphNodeEnv' m)

type GraphNodeEnv' m =
  { getTransferMeta :: GraphNodeEnv'getTransferMeta m
  , getTransactions :: GraphNodeEnv'getTransactions m
  }

type GraphNodeEnv'getTransferMeta m = TransferId -> m TransferMeta

type GraphNodeEnv'getTransactions m = { toAddress :: Address } -> m (Array Transfer)

--------------------------------------------------------------------------------
-- CirclesCoreEnv
--------------------------------------------------------------------------------

newtype CirclesCoreEnv m = CirclesCoreEnv (CirclesCoreEnv' m)

type CirclesCoreEnv' m =
  { getTrusts ::
      CirclesCoreEnv'getTrusts m
  , getPaymentNote ::
      CirclesCoreEnv'getPaymentNote m
  , trustAddConnection ::
      CirclesCoreEnv'trustAddConnection m
  }

type CirclesCoreEnv'getTrusts m = C.Address -> m (Set C.Address)

type CirclesCoreEnv'getPaymentNote m = String -> m String

type CirclesCoreEnv'trustAddConnection m = TrustAddConnectionOptions -> m String

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
-- Lenses
--------------------------------------------------------------------------------

_AppEnv :: forall m. Lens' (AppEnv m) (AppEnv' m)
_AppEnv = lens' (\(AppEnv x) -> x /\ AppEnv)

_CirclesCoreEnv :: forall m. Lens' (CirclesCoreEnv m) (CirclesCoreEnv' m)
_CirclesCoreEnv = lens' (\(CirclesCoreEnv x) -> x /\ CirclesCoreEnv)

_AppEnvVars :: Lens' AppEnvVars AppEnvVars'
_AppEnvVars = lens' (\(AppEnvVars x) -> x /\ AppEnvVars)

_GraphNodeEnv :: forall m. Lens' (GraphNodeEnv m) (GraphNodeEnv' m)
_GraphNodeEnv = lens' (\(GraphNodeEnv x) -> x /\ GraphNodeEnv)

modifyAppEnv :: forall m. (AppEnv' m -> AppEnv' m) -> AppEnv m -> AppEnv m
modifyAppEnv f (AppEnv r) = AppEnv $ f r

_getTrusts = Proxy :: Proxy "getTrusts"
_circlesCore = Proxy :: Proxy "circlesCore"

_envVars = Proxy :: Proxy "envVars"

_a :: forall m. Lens' (AppEnv m) (CirclesCoreEnv' m)
_a = _AppEnv <<< prop _circlesCore <<< _CirclesCoreEnv