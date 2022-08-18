module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (Address)
import Data.Lens (Lens', lens')
import Data.Tuple.Nested ((/\))
import Payload.Server.Response as Res
import Type.Proxy (Proxy(..))
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.Spec.Types (TransferId)
import VoucherServer.Types (TransferMeta)

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
  | ErrGraphQL
  | ErrGraphQLParse String

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
  ErrCirclesCore _ -> internalServerError
  ErrUnknown -> internalServerError
  ErrBasicAuth -> Error $ Res.internalError $ StringBody "Authorization failed"
  ErrGraphQL -> internalServerError
  ErrGraphQLParse _ -> internalServerError
  where
  internalServerError = Error $ Res.internalError $ StringBody "Internal server error"

errorToLog :: AppError -> String
errorToLog = case _ of
  ErrCirclesCore _ -> "Circles Core Error"
  ErrUnknown -> "Unknown error"
  ErrBasicAuth -> "Basic Authentication failed"
  ErrGraphQL -> "Graph QL Error"
  ErrGraphQLParse msg -> "Graph QL Parse Error: " <> msg

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

newtype AppEnv m = AppEnv (AppEnv' m)
newtype CirclesCoreEnv m = CirclesCoreEnv (CirclesCoreEnv' m)
newtype GraphNodeEnv m = GraphNodeEnv (GraphNodeEnv' m)

type AppEnv' m =
  { envVars :: AppEnvVars
  , graphNode :: GraphNodeEnv m
  , circlesCore :: CirclesCoreEnv m
  }

type GraphNodeEnv' m =
  { getTransferMeta :: GraphNodeEnv_getTransferMeta m
  }

type CirclesCoreEnv' m =
  { getTrusts :: CirclesCoreEnv_getTrusts m
  }

type CirclesCoreEnv_getTrusts m = Address -> m (Set Address)

type GraphNodeEnv_getTransferMeta m = TransferId -> m TransferMeta

--------------------------------------------------------------------------------

_AppEnv :: forall m. Lens' (AppEnv m) (AppEnv' m)
_AppEnv = lens' (\(AppEnv x) -> x /\ AppEnv)

_CirclesCoreEnv :: forall m. Lens' (CirclesCoreEnv m) (CirclesCoreEnv' m)
_CirclesCoreEnv = lens' (\(CirclesCoreEnv x) -> x /\ CirclesCoreEnv)

_GraphNodeEnv :: forall m. Lens' (GraphNodeEnv m) (GraphNodeEnv' m)
_GraphNodeEnv = lens' (\(GraphNodeEnv x) -> x /\ GraphNodeEnv)


modifyAppEnv :: forall m. (AppEnv' m -> AppEnv' m) -> AppEnv m -> AppEnv m
modifyAppEnv f (AppEnv r) = AppEnv $ f r

_getTrusts = Proxy :: Proxy "getTrusts"
_circlesCore = Proxy :: Proxy "circlesCore"