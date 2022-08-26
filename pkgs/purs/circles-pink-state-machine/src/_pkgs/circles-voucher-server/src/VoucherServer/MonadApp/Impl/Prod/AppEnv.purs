module VoucherServer.MonadApp.Impl.Prod.AppEnv where

import Prelude

import Control.Monad.Reader (ask)
import Effect.Class (liftEffect)
import Effect.Now as DT
import VoucherServer.MonadApp (AppEnv(..), AppProdM)
import VoucherServer.MonadApp.Class (AE'now)
import VoucherServer.MonadApp.Impl.Prod.CirclesCoreEnv (mkCirclesCoreEnv)
import VoucherServer.MonadApp.Impl.Prod.GraphNodeEnv (mkGraphNodeEnv)
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (MkAppProdM)
import VoucherServer.MonadApp.Impl.Prod.XbgeClientEnv (mkXbgeClientEnv)

type M a = MkAppProdM a

type N a = AppProdM a

mkAppEnv :: M (AppEnv AppProdM)
mkAppEnv = do
  { envVars, constants } <- ask
  circlesCore <- mkCirclesCoreEnv
  graphNode <- mkGraphNodeEnv
  xbgeClient <- mkXbgeClientEnv

  let
    now :: AE'now N
    now = liftEffect DT.now

  pure $ AppEnv
    { envVars
    , constants
    , graphNode
    , circlesCore
    , xbgeClient
    , now
    }
