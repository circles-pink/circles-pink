module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Newtype (un)
import Payload.ResponseTypes (Response(..))
import VoucherServer.Types.AppError (AppError)
import VoucherServer.Types.AppLog (AppLog)
import VoucherServer.Types.Envs (AppEnv)

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class
  ( MonadError AppError m
  , MonadAsk (AppEnv m) m
  ) <=
  MonadApp m where
  log :: AppLog -> m Unit

getResponseData :: forall r a. Response { data :: a | r } -> a
getResponseData = un Response >>> _.body >>> _.data
