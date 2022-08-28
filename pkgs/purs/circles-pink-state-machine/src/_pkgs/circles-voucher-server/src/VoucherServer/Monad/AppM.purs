module VoucherServer.Monad.AppM where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as E
import Payload.ResponseTypes (Response(..))
import VoucherServer.MonadApp.Class (class MonadApp)
import VoucherServer.Types.AppError (AppError)
import VoucherServer.Types.AppLog (logToString)
import VoucherServer.Types.Envs (AppEnv)

newtype AppM a = AppM
  ( ReaderT (AppEnv AppM)
      (ExceptT AppError Aff)
      a
  )

type M = AppM

derive instance Newtype (AppM a) _
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Functor AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadThrow AppError AppM
derive newtype instance MonadError AppError AppM
derive newtype instance MonadAsk (AppEnv AppM) AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM

instance monadVoucherServerAffAPM :: MonadApp AppM where
  log = logToString >>> E.log

runAppM :: forall a. AppEnv M -> M a -> Aff (Either AppError a)
runAppM env (AppM x) = runExceptT $ runReaderT x env

mapResponse :: forall a b. (a -> b) -> Response a -> Response b
mapResponse f (Response r) = Response r { body = f r.body }

