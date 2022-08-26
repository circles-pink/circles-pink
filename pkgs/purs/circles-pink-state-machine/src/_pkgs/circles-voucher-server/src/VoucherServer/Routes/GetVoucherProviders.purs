module VoucherServer.Routes.GetVoucherProviders where

import Prelude

import Control.Monad.Reader (ask)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv(..), getResponseData)
import VoucherServer.Spec.Types (VoucherProvider)

routeGetVoucherProviders
  :: forall m
   . MonadApp m
  => {}
  -> m (Response (Array VoucherProvider))
routeGetVoucherProviders {} = do
  AppEnv { xbgeClient: { getVoucherProviders } } <- ask

  voucherProviders <- getVoucherProviders {}
    <#> getResponseData

  pure $ Res.ok voucherProviders