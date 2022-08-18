module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (Address)
import Data.Lens (Lens', lens')
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Payload.Server.Response as Res
import Type.Proxy (Proxy(..))
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.Spec.Types (TransferId(..))
import VoucherServer.Types (TransferMeta(..))

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class
  ( MonadThrow AppError m
  , MonadAsk (AppEnv m) m
  ) <=
  MonadApp m

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

data AppError
  = ErrCirclesCore CCErrAll
  | ErrUnknown
  | ErrBasicAuth

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
      + ()
  )

errorToFailure :: AppError -> Failure
errorToFailure = case _ of
  ErrCirclesCore _ -> Error $ Res.internalError $ StringBody "Internal server error"
  ErrUnknown -> Error $ Res.internalError $ StringBody "Internal server error"
  ErrBasicAuth -> Error $ Res.internalError $ StringBody "Authorization failed"

errorToLog :: AppError -> String
errorToLog = case _ of
  ErrCirclesCore _ -> "Circles Core Error"
  ErrUnknown -> "Unknown error"
  ErrBasicAuth -> "Basic Authentication failed"

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

newtype AppEnv m = AppEnv (AppEnv' m)

type AppEnv' m =
  { envVars :: AppEnvVars
  , getTrusts :: AppEnv_getTrusts m
  , graphNode :: AppEnv_graphNode m
  }

type AppEnv_getTrusts m = Address -> m (Set Address)

type AppEnv_graphNode m =
  { -- getTransferMeta :: AppEnv_graphNode_getTransferMeta m
  }

type AppEnv_graphNode_getTransferMeta m = TransferId -> m TransferMeta

--------------------------------------------------------------------------------

_AppEnv :: forall m. Lens' (AppEnv m) (AppEnv' m)
_AppEnv = lens' (\(AppEnv x) -> x /\ AppEnv)

modifyAppEnv :: forall m. (AppEnv' m -> AppEnv' m) -> AppEnv m -> AppEnv m
modifyAppEnv f (AppEnv r) = AppEnv $ f r

_getTrusts :: forall m. Lens' (AppEnv m) (AppEnv_getTrusts m)
_getTrusts = _AppEnv <<< prop (Proxy :: _ "getTrusts")