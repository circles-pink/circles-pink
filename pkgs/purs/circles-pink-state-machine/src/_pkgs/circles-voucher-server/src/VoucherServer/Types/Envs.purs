module VoucherServer.Types.Envs where

import VoucherServer.Prelude

import CirclesCore (TrustAddConnectionOptions)
import CirclesCore as CC
import CirclesPink.Data.Address as C
import Data.DateTime.Instant (Instant)
import Data.Lens (Lens', lens')
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Payload.ResponseTypes (Response)
import Type.Proxy (Proxy(..))
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.Spec.Types (TransferId, VoucherAmount, VoucherEncrypted, VoucherProvider, VoucherProviderId)
import VoucherServer.Specs.Xbge (Address)
import VoucherServer.Types (Transfer, TransferMeta)
import VoucherServer.Types.AppConstants (AppConstants)

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

newtype CirclesCoreEnv m = CirclesCoreEnv
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

newtype XbgeClientEnv m = XbgeClientEnv
  { getVoucherProviders ::
      XBG'getVoucherProviders m
  , finalizeVoucherPurchase ::
      XBG'finalizeVoucherPurchase m
  , getVouchers ::
      XBG'getVouchers m
  }

newtype GraphNodeEnv m = GraphNodeEnv
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
-- Lenses
--------------------------------------------------------------------------------

_AppEnv :: Lens' _ _
_AppEnv = lens' (\(AppEnv x) -> x /\ AppEnv)

_CirclesCoreEnv :: Lens' _ _

_CirclesCoreEnv = lens' (\(CirclesCoreEnv x) -> x /\ CirclesCoreEnv)

-- _AppEnvVars :: Lens' AppEnvVars _
-- _AppEnvVars = lens' (\(AppEnvVars x) -> x /\ AppEnvVars)

_GraphNodeEnv :: Lens' _ _

_GraphNodeEnv = lens' (\(GraphNodeEnv x) -> x /\ GraphNodeEnv)

_getTrusts = Proxy :: Proxy "getTrusts"
_circlesCore = Proxy :: Proxy "circlesCore"

_envVars = Proxy :: Proxy "envVars"