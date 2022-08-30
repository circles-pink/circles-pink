module VoucherServer.Monad.Init
  ( InitM
  , runInitM
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import VoucherServer.Types.AppConstants (AppConstants)
import VoucherServer.Types.AppError (AppError)

newtype InitM a = InitM
  ( ReaderT AppConstants
      (ExceptT AppError Aff)
      a
  )

derive newtype instance Apply InitM
derive newtype instance Applicative InitM
derive newtype instance Functor InitM
derive newtype instance Bind InitM
derive newtype instance Monad InitM
derive newtype instance MonadThrow AppError InitM
derive newtype instance MonadError AppError InitM
derive newtype instance MonadAsk AppConstants InitM
derive newtype instance MonadEffect InitM
derive newtype instance MonadAff InitM

runInitM :: forall a. AppConstants -> InitM a -> Aff (Either AppError a)
runInitM env (InitM x) = runExceptT $ runReaderT x env

