module VoucherServer.MonadApp.Impl.Prod.AppEnv where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Data.Either (Either)
import Debug.Extra (todo)
import Effect.Aff (Aff)
import VoucherServer.EnvVars (AppEnvVars)
import VoucherServer.MonadApp (AppEnv(..), AppError, AppProdM)
import VoucherServer.MonadApp.Impl.Prod.CirclesCoreEnv (mkCirclesCoreEnv)
import VoucherServer.MonadApp.Impl.Prod.GraphNodeEnv (mkGraphNodeEnv)
import VoucherServer.MonadApp.Impl.Prod.XbgeClientEnv (mkXbgeClientEnv)

type M a = ReaderT AppEnvVars (ExceptT AppError Aff) a

mkAppEnv :: M (AppEnv AppProdM)
mkAppEnv = do
  envVars <- ask
  circlesCore <- mkCirclesCoreEnv
  graphNode <- mkGraphNodeEnv
  xbgeClient <- mkXbgeClientEnv

  pure $ AppEnv
    { envVars
    , graphNode
    , circlesCore
    , xbgeClient
    }


runM :: forall a. AppEnvVars -> M a -> Aff (Either AppError a)
runM = todo
