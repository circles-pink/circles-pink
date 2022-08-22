module VoucherServer.Routes.TrustUsers where

import Prelude

import CirclesPink.Data.Address (Address)
import Control.Monad.Reader (ask)
import Convertable (convert)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp (class MonadApp, AppEnv(..))
import VoucherServer.MonadApp.Class (CirclesCoreEnv(..))

trustUsers :: forall m. MonadApp m => { body :: { safeAddresses :: Array Address } } -> m {}
trustUsers { body: { safeAddresses } } = do
  _ <- for safeAddresses trustUser
  pure {}

trustUser :: forall m. MonadApp m => Address -> m String
trustUser safeAddress = do
  AppEnv
    { circlesCore: CirclesCoreEnv { trustAddConnection }
    , envVars: AppEnvVars { xbgeSafeAddress }
    } <- ask

  trustAddConnection
    { user: convert safeAddress
    , canSendTo: convert $ unwrap xbgeSafeAddress
    , limitPercentage: 100.0
    }
