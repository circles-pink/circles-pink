module VoucherServer.Guards.Auth
  ( basicAuthGuard
  )
  where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Foreign.Object as Object
import Node.HTTP as HTTP
import Payload.Headers (Headers)
import Payload.Headers as Headers
import Payload.Server.Response as Res
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv(..), AppError(..))

basicAuthGuard :: forall m. MonadApp m => HTTP.Request -> m Unit
basicAuthGuard req = do
  AppEnv {envVars} <- ask
  let headers = getHeaders req
  case Headers.lookup "Authorization" headers of
    Just tok | tok == ("Basic " <> envVars.voucherServerBasicAuth) -> pure unit
    _ -> throwError $ Res.unauthorized ErrBasicAuth

getHeaders :: HTTP.Request -> Headers
getHeaders req = Headers.fromFoldable headersArr
  where
  headersArr :: Array (String /\ String)
  headersArr = Object.toUnfoldable $ HTTP.requestHeaders req