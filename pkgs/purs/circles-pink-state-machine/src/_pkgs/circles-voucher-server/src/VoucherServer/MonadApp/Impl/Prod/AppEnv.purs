module VoucherServer.MonadApp.Impl.Prod.AppEnv where

import Prelude

import Control.Monad.Reader (ask)
import VoucherServer.MonadApp (AppEnv(..), AppProdM)
import VoucherServer.MonadApp.Impl.Prod.CirclesCoreEnv (mkCirclesCoreEnv)
import VoucherServer.MonadApp.Impl.Prod.GraphNodeEnv (mkGraphNodeEnv)
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (MkAppProdM)
import VoucherServer.MonadApp.Impl.Prod.XbgeClientEnv (mkXbgeClientEnv)

type M a = MkAppProdM a

mkAppEnv :: M (AppEnv AppProdM)
mkAppEnv = do
  { envVars, constants } <- ask
  circlesCore <- mkCirclesCoreEnv
  graphNode <- mkGraphNodeEnv
  xbgeClient <- mkXbgeClientEnv

  pure $ AppEnv
    { envVars
    , constants
    , graphNode
    , circlesCore
    , xbgeClient
    }
