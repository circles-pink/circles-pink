module VoucherServer.Monad.MkAppM
  ( Env
  , MkAppM
  , runMkAppM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.Types.AppConstants (AppConstants)
import VoucherServer.Types.AppError (AppError)

newtype MkAppM a = MkAppM (ReaderT Env (ExceptT AppError Aff) a)

derive newtype instance Apply MkAppM
derive newtype instance Applicative MkAppM
derive newtype instance Functor MkAppM
derive newtype instance Bind MkAppM
derive newtype instance Monad MkAppM
derive newtype instance MonadThrow AppError MkAppM
derive newtype instance MonadError AppError MkAppM
derive newtype instance MonadAsk Env MkAppM
derive newtype instance MonadEffect MkAppM
derive newtype instance MonadAff MkAppM

type Env =
  { envVars :: AppEnvVars
  , constants :: AppConstants
  }

runMkAppM :: forall a. Env -> MkAppM a -> Aff (Either AppError a)
runMkAppM env (MkAppM x) = runExceptT $ runReaderT x env
