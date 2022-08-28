module VoucherServer.Routes.GetVoucherProviders where

import Prelude

import Control.Monad.Reader (ask)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import VoucherServer.MonadApp.Class (class MonadApp, getResponseData)
import VoucherServer.Spec.Types (VoucherProvider)
import VoucherServer.Types.Envs (AppEnv(..), XbgeClientEnv(..))

routeGetVoucherProviders
  :: forall m
   . MonadApp m
  => {}
  -> m (Response (Array VoucherProvider))
routeGetVoucherProviders {} = do
  AppEnv { xbgeClient: XbgeClientEnv { getVoucherProviders } } <- ask

  voucherProviders <- getVoucherProviders {}
    <#> getResponseData

  pure $ Res.ok voucherProviders