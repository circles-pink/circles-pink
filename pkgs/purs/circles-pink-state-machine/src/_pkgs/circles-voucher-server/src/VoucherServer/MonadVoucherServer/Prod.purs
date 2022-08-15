module VoucherServer.MonadVoucherServer.Prod where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Payload.ResponseTypes (Failure)
import VoucherServer.MonadVoucherServer (class MonadVoucherServer, VoucherServerError, apiErrorToFailure, apiErrorToLog)

newtype VoucherServerProdM a = VoucherServerProdM (ExceptT VoucherServerError Aff a)

derive instance newtypeVoucherServerProdM :: Newtype (VoucherServerProdM a) _
derive newtype instance applyVoucherServerProdM :: Apply VoucherServerProdM
derive newtype instance applicativeVoucherServerProdM :: Applicative VoucherServerProdM
derive newtype instance functorVoucherServerProdM :: Functor VoucherServerProdM
derive newtype instance bindVoucherServerProdM :: Bind VoucherServerProdM
derive newtype instance monadVoucherServerProdM :: Monad VoucherServerProdM
derive newtype instance monadThrowVoucherServerProdM :: MonadThrow VoucherServerError VoucherServerProdM

runVoucherServerProdM :: forall a. VoucherServerProdM a -> Aff (Either Failure a)
runVoucherServerProdM (VoucherServerProdM x) = do 
  result <- runExceptT x
  case result of
    Left err -> do
      log ("ERROR: " <> apiErrorToLog err)
      pure $ Left $ apiErrorToFailure err
    Right y -> pure $ Right y

instance i :: MonadVoucherServer VoucherServerProdM