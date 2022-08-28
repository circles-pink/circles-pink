module VoucherServer.Monad.AppM.XbgeClientEnv where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader (ask)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Payload.Client (ClientError, mkClient)
import Payload.Client as PC
import Payload.Headers as H
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.Monad.AppM (AppM)
import VoucherServer.Monad.MkAppM (MkAppM)
import VoucherServer.Specs.Xbge (xbgeSpec)
import VoucherServer.Types.AppError (AppError(..))
import VoucherServer.Types.Envs (XbgeClientEnv(..))

type M a = MkAppM a

mkXbgeClientEnv :: M (XbgeClientEnv AppM)
mkXbgeClientEnv = do
  { envVars: AppEnvVars envVars } <- ask
  let
    options = PC.defaultOpts
      { baseUrl = envVars.xbgeEndpoint
      , extraHeaders = H.fromFoldable [ "Authorization" /\ ("Basic " <> envVars.xbgeAuthSecret) ]
      -- , logLevel = Log
      }
    xbgeClient = mkClient options xbgeSpec
  pure $ XbgeClientEnv
    { getVoucherProviders: xbgeClient.getVoucherProviders
        >>> liftPayload
    , finalizeVoucherPurchase: xbgeClient.finalizeVoucherPurchase
        >>> liftPayload
    , getVouchers: xbgeClient.getVouchers
        >>> liftPayload
    }

liftPayload :: forall a. Aff (Either ClientError a) -> AppM a
liftPayload x = x
  # liftAff
  <#> lmap ErrPayloadClient
  >>= liftEither