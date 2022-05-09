module CirclesPink.EnvVars where

import Prelude
import Data.Either (Either)
import Effect (Effect)
import Node.Process (getEnv)
import Type.Proxy (Proxy(..))
import TypedEnv (EnvError, Resolved, Variable, envErrorMessage)
import TypedEnv (fromEnv) as TypedEnv

type Config :: forall k. (Symbol -> Type -> k) -> Row k
type Config f
  = ( gardenApi :: f "GARDEN_API" String
    , gardenApiUsers :: f "GARDEN_API_USERS" String
    , gardenGraphApi :: f "GARDEN_GRAPH_API" String
    , gardensubgraphName :: f "GARDEN_SUBGRAPH_NAME" String
    , gardenRelay :: f "GARDEN_RELAY" String
    , gardenHubAdress :: f "GARDEN_HUB_ADDRESS" String
    , gardenProxyFactoryAdress :: f "GARDEN_PROXY_FACTORY_ADRESS" String
    , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" String
    , gardenEthereumNodeWs :: f "GARDEN_ETHEREUM_NODE_WS" String
    )

type ResolvedConfig
  = Record (Config Resolved)

parse :: Effect (Either EnvError ResolvedConfig)
parse = TypedEnv.fromEnv (Proxy :: _ (Config Variable)) <$> getEnv
