module VoucherServer.Routes.TrustCount
  ( trustCount
  ) where

import Prelude

import CirclesCore.Bindings (TrustIsTrustedResult(..))
import CirclesPink.Data.Address (Address)
import Control.Monad.Reader (ask)
import Convertable (convert)
import Data.Traversable (for)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import VoucherServer.MonadApp (class MonadApp, AppEnv(..))
import VoucherServer.MonadApp.Class (CirclesCoreEnv(..))

--------------------------------------------------------------------------------
-- Route
--------------------------------------------------------------------------------

trustCount
  :: forall m
   . MonadApp m
  => { guards :: { basicAuth :: Unit }
     , body :: { safeAddresses :: Array Address }
     }
  -> m (Response (Array { safeAddress :: Address, trustConnections :: Int }))
trustCount { body: { safeAddresses } } = do
  AppEnv
    { circlesCore: CirclesCoreEnv { trustIsTrusted }
    } <- ask

  results <- for safeAddresses \safeAddress -> do
    TrustIsTrustedResult { trustConnections } <- trustIsTrusted
      { limit: 3
      , safeAddress: convert safeAddress
      }
    pure { safeAddress, trustConnections }

  pure $ Res.ok results
