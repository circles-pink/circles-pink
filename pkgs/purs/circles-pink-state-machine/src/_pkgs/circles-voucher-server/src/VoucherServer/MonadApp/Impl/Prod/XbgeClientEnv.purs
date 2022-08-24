module VoucherServer.MonadApp.Impl.Prod.XbgeClientEnv where

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
import VoucherServer.MonadApp (AppError(..), AppProdM)
import VoucherServer.MonadApp.Class (XbgeClientEnv)
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (MkAppProdM)
import VoucherServer.Specs.Xbge (xbgeSpec)

type M a = MkAppProdM a

mkXbgeClientEnv :: M (XbgeClientEnv AppProdM)
mkXbgeClientEnv = do
  { envVars : AppEnvVars env} <- ask
  let
    options = PC.defaultOpts
      { baseUrl = env.xbgeEndpoint
      , extraHeaders = H.fromFoldable [ "Authorization" /\ ("Basic " <> env.xbgeAuthSecret) ]
      -- , logLevel = Log
      }
    xbgeClient = mkClient options xbgeSpec
  pure
    { getVoucherProviders: xbgeClient.getVoucherProviders
        >>> liftPayload
    , finalizeVoucherPurchase: xbgeClient.finalizeVoucherPurchase
        >>> liftPayload
    , getVouchers: xbgeClient.getVouchers
        >>> liftPayload
    }

liftPayload :: forall a. Aff (Either ClientError a) -> AppProdM a
liftPayload x = x
  # liftAff
  <#> lmap ErrPayloadClient
  >>= liftEither