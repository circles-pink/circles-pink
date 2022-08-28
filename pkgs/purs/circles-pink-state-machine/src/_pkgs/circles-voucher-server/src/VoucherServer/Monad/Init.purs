module VoucherServer.Monad.Init where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT(..))
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Class.Console as E
import VoucherServer.Types.AppConstants (AppConstants(..))
import VoucherServer.Types.AppError (AppError, errorToLog)

newtype InitM a =  InitM 
  ( ReaderT AppConstants
      (ExceptT AppError Aff)
      a
  )

derive instance Newtype (InitM a) _

derive newtype instance Apply InitM
derive newtype instance Applicative InitM
derive newtype instance Functor InitM
derive newtype instance Bind InitM
derive newtype instance Monad InitM
derive newtype instance MonadThrow AppError InitM
derive newtype instance MonadError AppError InitM



runInitM :: forall a. InitM a -> Aff Unit
runInitM (InitM x) = do
  result <- runExceptT x
  case result of
    Left err ->
      E.log ("Server Inititialization error: " <> errorToLog err)
    Right _ -> pure unit
