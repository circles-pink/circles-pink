module VoucherServer.Routes.TrustUsers where

import Prelude

import CirclesPink.Data.Address (Address)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ask)
import Convertable (convert)
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp (class MonadApp, AppEnv(..))
import VoucherServer.MonadApp.Class (CirclesCoreEnv(..))

trustUsers
  :: forall m
   . MonadApp m
  => { guards :: { basicAuth :: Unit }
     , body :: { safeAddresses :: Array Address }
     }
  -> m {}
trustUsers { body: { safeAddresses } } = do

  for_ safeAddresses (\sa -> (void $ trustUser sa) `catchError` (\_ -> pure unit))
  pure {}

trustUser :: forall m. MonadApp m => Address -> m String
trustUser safeAddress = do
  AppEnv
    { circlesCore: CirclesCoreEnv { trustAddConnection }
    , envVars: AppEnvVars { xbgeSafeAddress }
    , constants: { trustLimitPercentage }
    } <- ask

  trustAddConnection
    { user: convert safeAddress
    , canSendTo: convert $ unwrap xbgeSafeAddress
    , limitPercentage: trustLimitPercentage
    }
