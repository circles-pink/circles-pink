module VoucherServer.Routes.TrustUsers where

import Prelude

import CirclesPink.Data.Address (Address)
import Control.Monad.Reader (asks)
import Convertable (convert)
import Data.Lens (view)
import Data.Lens.Record (prop)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp (class MonadApp)
import VoucherServer.MonadApp.Class (CirclesCoreEnv(..), _AppEnv, _circlesCore, _envVars)

trustUsers :: forall m. MonadApp m => { body :: { safeAddresses :: Array Address }} -> m {}
trustUsers {body : {safeAddresses}} = do  
  _ <- for safeAddresses trustUser
  pure {}

trustUser :: forall m. MonadApp m => Address -> m String
trustUser safeAddress = do
  CirclesCoreEnv circlesCore <- asks $ view $ _AppEnv <<< prop _circlesCore
  AppEnvVars envVars <- asks $ view $ _AppEnv <<< prop _envVars

  circlesCore.trustAddConnection
    { user: convert safeAddress
    , canSendTo: convert $ unwrap envVars.xbgeSafeAddress
    , limitPercentage: 100.0
    }
