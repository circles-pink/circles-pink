module VoucherServer.Monad.MkAppM where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.Types.AppConstants (AppConstants)
import VoucherServer.Types.AppError (AppError)


type MkAppM a = ReaderT Env (ExceptT AppError Aff) a

type Env =
  { envVars :: AppEnvVars
  , constants :: AppConstants
  }

runMkAppM :: forall a. Env -> MkAppM a -> Aff (Either AppError a)
runMkAppM env x = runExceptT $ runReaderT x env
