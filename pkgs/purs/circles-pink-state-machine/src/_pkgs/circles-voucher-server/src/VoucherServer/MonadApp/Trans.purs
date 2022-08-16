module VoucherServer.MonadAppApp.Trans where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Payload.ResponseTypes (Response)
import VoucherServer.MonadApp.Class (AppEnv, AppError)

type Error = Response AppError

type Env m = AppEnv (AppT m)

newtype AppT m a = AppT
  ( ReaderT (Env m)
      (ExceptT Error m)
      a
  )