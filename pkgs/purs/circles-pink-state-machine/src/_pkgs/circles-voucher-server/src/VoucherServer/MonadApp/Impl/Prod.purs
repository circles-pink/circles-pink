module VoucherServer.MonadApp.Impl.Prod where

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
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv, AppError, logToString)

newtype AppProdM a = AppProdM
  ( ReaderT (AppEnv AppProdM)
      (ExceptT AppError Aff)
      a
  )

type M = AppProdM

derive instance Newtype (AppProdM a) _
derive newtype instance Apply AppProdM
derive newtype instance Applicative AppProdM
derive newtype instance Functor AppProdM
derive newtype instance Bind AppProdM
derive newtype instance Monad AppProdM
derive newtype instance MonadThrow AppError AppProdM
derive newtype instance MonadError AppError AppProdM
derive newtype instance MonadAsk (AppEnv AppProdM) AppProdM
derive newtype instance MonadEffect AppProdM
derive newtype instance MonadAff AppProdM

instance monadVoucherServerAffAPM :: MonadApp AppProdM where
  log = logToString >>> E.log

runAppProdM :: forall a. AppEnv M -> M a -> Aff (Either AppError a)
runAppProdM env (AppProdM x) = runExceptT $ runReaderT x env

mapResponse :: forall a b. (a -> b) -> Response a -> Response b
mapResponse f (Response r) = Response r { body = f r.body }

