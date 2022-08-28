module VoucherServer.Monad.AppM.AppEnv where

import Prelude

import Control.Monad.Reader (ask)
import Effect.Class (liftEffect)
import Effect.Now as DT
import VoucherServer.Monad.AppM (AppM(..))
import VoucherServer.Monad.AppM.CirclesCoreEnv (mkCirclesCoreEnv)
import VoucherServer.Monad.AppM.GraphNodeEnv (mkGraphNodeEnv)
import VoucherServer.Monad.AppM.XbgeClientEnv (mkXbgeClientEnv)
import VoucherServer.Monad.MkAppM (MkAppM)
import VoucherServer.Types.Envs (AE'now, AppEnv(..))

type M a = MkAppM a

type N a = AppM a

mkAppEnv :: M (AppEnv AppM)
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
