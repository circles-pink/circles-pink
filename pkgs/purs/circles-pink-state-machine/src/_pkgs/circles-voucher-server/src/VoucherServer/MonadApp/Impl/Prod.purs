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
import Payload.ResponseTypes (Response(..))
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv, AppError)

newtype AppProdM a = AppProdM
  ( ReaderT (AppEnv AppProdM)
      (ExceptT AppError Aff)
      a
  )

type M = AppProdM

derive instance newtypeAPM :: Newtype (AppProdM a) _
derive newtype instance applyAPM :: Apply AppProdM
derive newtype instance applicativeAPM :: Applicative AppProdM
derive newtype instance functorAPM :: Functor AppProdM
derive newtype instance bindAPM :: Bind AppProdM
derive newtype instance monadAPM :: Monad AppProdM
derive newtype instance monadThrowAPM :: MonadThrow AppError AppProdM
derive newtype instance monadErrorAPM :: MonadError AppError AppProdM
derive newtype instance monadAskAPM :: MonadAsk (AppEnv AppProdM) AppProdM
derive newtype instance monadEffectAPM :: MonadEffect AppProdM
derive newtype instance monadAffAPM :: MonadAff AppProdM
instance monadVoucherServerAffAPM :: MonadApp AppProdM

runAppProdM :: forall a. AppEnv M -> M a -> Aff (Either AppError a)
runAppProdM env (AppProdM x) = runExceptT $ runReaderT x env

mapResponse :: forall a b. (a -> b) -> Response a -> Response b
mapResponse f (Response r) = Response r { body = f r.body }

