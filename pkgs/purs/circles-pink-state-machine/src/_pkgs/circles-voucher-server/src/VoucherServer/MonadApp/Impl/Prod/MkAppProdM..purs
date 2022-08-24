module VoucherServer.MonadApp.Impl.Prod.MkAppProdM where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.MonadApp (AppError)
import VoucherServer.MonadApp.Class (AppConstants)

type MkAppProdM a = ReaderT Env (ExceptT AppError Aff) a

type Env =
  { envVars :: AppEnvVars
  , constants :: AppConstants
  }

runMkAppProdM :: forall a. Env -> MkAppProdM a -> Aff (Either AppError a)
runMkAppProdM env x = runExceptT $ runReaderT x env
