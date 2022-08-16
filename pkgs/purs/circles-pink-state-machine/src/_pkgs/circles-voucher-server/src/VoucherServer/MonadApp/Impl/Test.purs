module VoucherServer.MonadApp.Impl.Test where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.PrivateKey.Type (PrivateKey(..))
import Control.Monad.Error.Class (class MonadThrow, liftEither, throwError)
import Control.Monad.Except (ExceptT, mapExceptT, runExceptT, withExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Convertable (convert)
import Data.Array as A
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Newtype (class Newtype, wrap)
import Data.Set as Set
import Debug.Extra (todo)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Payload.ResponseTypes (Failure, Response)
import Safe.Coerce (coerce)
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.MonadApp.Class (class MonadApp, AppEnv(..), AppError(..), apiErrorToFailure, apiErrorToLog)

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype AppTestM a = AppTestM
  ( ReaderT (AppEnv AppTestM)
      (ExceptT AppError Identity)
      a
  )

derive instance newtypeATM ::  Newtype (AppTestM a) _
derive newtype instance applyATM ::  Apply AppTestM
derive newtype instance applicATMiveATM ::  Applicative AppTestM
derive newtype instance functorATM ::  Functor AppTestM
derive newtype instance bindATM ::  Bind AppTestM
derive newtype instance monadATM ::  Monad AppTestM
derive newtype instance monadThrowATM ::  MonadThrow AppError AppTestM
derive newtype instance monadAskATM ::  MonadAsk (AppEnv AppTestM) AppTestM

instance monadAppATM :: MonadApp AppTestM


--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------


testEnv :: AppEnv AppTestM
testEnv = todo --sample

runAppTestM :: forall a. AppEnv AppTestM -> AppTestM a -> (Either (Response AppError) a)
runAppTestM = todo
-- runAppTestM env (AppTestM x) = do
--   result <- runExceptT $ runReaderT x env
--   case result of
--     Left err -> do
--       log ("ERROR: " <> apiErrorToLog err)
--       pure $ Left $ apiErrorToFailure err
--     Right y -> pure $ Right y

