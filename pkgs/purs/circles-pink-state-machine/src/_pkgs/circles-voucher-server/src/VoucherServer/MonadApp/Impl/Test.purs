module VoucherServer.MonadApp.Impl.Test where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, un)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv(..), AppError(..))

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype AppTestM a = AppTestM
  ( ReaderT (AppEnv AppTestM)
      (ExceptT (Response AppError) Identity)
      a
  )

derive instance newtypeATM :: Newtype (AppTestM a) _
derive newtype instance applyATM :: Apply AppTestM
derive newtype instance applicATMiveATM :: Applicative AppTestM
derive newtype instance functorATM :: Functor AppTestM
derive newtype instance bindATM :: Bind AppTestM
derive newtype instance monadATM :: Monad AppTestM
derive newtype instance monadThrowATM :: MonadThrow (Response AppError) AppTestM
derive newtype instance monadAskATM :: MonadAsk (AppEnv AppTestM) AppTestM

instance monadAppATM :: MonadApp AppTestM

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

testEnv :: AppEnv AppTestM
testEnv = AppEnv
  { getTrusts: \_ -> throwError $ Res.internalError ErrUnknown
  , envVars: evalGen arbitrary { newSeed: mkSeed 0, size: 100 }
  }

runAppTestM :: forall a. AppEnv AppTestM -> AppTestM a -> Either (Response AppError) a
runAppTestM env (AppTestM x) = un Identity do
  result <- runExceptT $ runReaderT x env
  case result of
    Left res -> do
      pure $ Left $ res
    Right y -> pure $ Right y

