module VoucherServer.Monad.AppTestM
  ( AppTestM(..)
  , runAppTestM
  , testEnv
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, un)
import Test.QuickCheck (class Arbitrary, arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import VoucherServer.MonadApp.Class (class MonadApp)
import VoucherServer.Types.AppError (AppError(..))
import VoucherServer.Types.Envs (AppEnv(..), CirclesCoreEnv(..), GraphNodeEnv(..), XbgeClientEnv(..))

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype AppTestM a = AppTestM
  ( ReaderT (AppEnv AppTestM)
      (ExceptT AppError Identity)
      a
  )

derive instance Newtype (AppTestM a) _
derive newtype instance Apply AppTestM
derive newtype instance Applicative AppTestM
derive newtype instance Functor AppTestM
derive newtype instance Bind AppTestM
derive newtype instance Monad AppTestM
derive newtype instance MonadThrow AppError AppTestM
derive newtype instance MonadError AppError AppTestM
derive newtype instance MonadAsk (AppEnv AppTestM) AppTestM

instance MonadApp AppTestM where
  log _ = pure unit
  scope _ = identity

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

testEnv :: AppEnv AppTestM
testEnv = AppEnv
  { envVars: gen
  , graphNode: GraphNodeEnv
      { getTransferMeta: fail
      , getTransactions: fail
      }
  , circlesCore: CirclesCoreEnv
      { getTrusts: fail
      , getPaymentNote: fail
      , trustAddConnection: fail
      , trustIsTrusted: fail
      , getSafeAddress: fail
      }
  , xbgeClient: XbgeClientEnv
      { getVoucherProviders: fail
      , finalizeVoucherPurchase: fail
      , getVouchers: fail
      }
  , constants: gen
  , now: pure bottom
  }
  where
  fail :: forall b a. b -> AppTestM a
  fail = \_ ->
    throwError ErrUnknown

gen :: forall a. Arbitrary a => a
gen = evalGen arbitrary { newSeed: mkSeed 0, size: 100 }

runAppTestM :: forall a. AppEnv AppTestM -> AppTestM a -> Either AppError a
runAppTestM env (AppTestM x) = un Identity do
  result <- runExceptT $ runReaderT x env
  case result of
    Left res -> do
      pure $ Left $ res
    Right y -> pure $ Right y

