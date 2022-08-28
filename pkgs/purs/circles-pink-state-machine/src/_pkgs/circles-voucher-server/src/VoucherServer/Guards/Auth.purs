module VoucherServer.Guards.Auth
  ( basicAuthGuard
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Foreign.Object as Object
import Node.HTTP as HTTP
import Payload.Headers (Headers)
import Payload.Headers as Headers
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp.Class (class MonadApp)
import VoucherServer.Types.AppError (AppError(..))
import VoucherServer.Types.Envs (AppEnv(..))

--------------------------------------------------------------------------------
-- Guard
--------------------------------------------------------------------------------

basicAuthGuard :: forall m. MonadApp m => HTTP.Request -> m Unit
basicAuthGuard req = do
  AppEnv { envVars: AppEnvVars { voucherServerBasicAuth } } <- ask
  let headers = getHeaders req
  case Headers.lookup "Authorization" headers of
    Just tok | tok == ("Basic " <> voucherServerBasicAuth) -> pure unit
    _ -> throwError ErrBasicAuth

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

getHeaders :: HTTP.Request -> Headers
getHeaders req = Headers.fromFoldable headersArr
  where
  headersArr :: Array (String /\ String)
  headersArr = Object.toUnfoldable $ HTTP.requestHeaders req