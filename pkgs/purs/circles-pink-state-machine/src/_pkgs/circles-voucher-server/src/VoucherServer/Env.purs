module VoucherServer.Env where


import Data.Maybe (Maybe)
import TypedEnv (Variable, Resolved)
import VoucherServer.Specs.Xbge (Address(..))

--------------------------------------------------------------------------------

type ServerConfigSpec :: forall k. (Symbol -> Type -> k) -> Row k
type ServerConfigSpec f =
  ( port :: f "PORT" (Maybe Int)
  , gardenApi :: f "GARDEN_API" String
  , gardenApiUsers :: f "GARDEN_API_USERS" String
  , gardenGraphApi :: f "GARDEN_GRAPH_API" String
  , gardenSubgraphName :: f "GARDEN_SUBGRAPH_NAME" String
  , gardenRelay :: f "GARDEN_RELAY" String
  , gardenHubAddress :: f "GARDEN_HUB_ADDRESS" String
  , gardenProxyFactoryAddress :: f "GARDEN_PROXY_FACTORY_ADRESS" String
  , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" String
  , gardenEthereumNodeWebSocket :: f "GARDEN_ETHEREUM_NODE_WS" String
  , voucherCodeSecret :: f "VOUCHER_CODE_SECRET" String
  , xbgeAuthSecret :: f "XBGE_AUTH_SECRET" String
  , xbgeEndpoint :: f "XBGE_ENDPOINT" String
  , xbgeSafeAddress :: f "XBGE_SAFE_ADDRESS" Address
  )

type ServerConfig = ServerConfigSpec Variable

type ServerEnv = { | ServerConfigSpec Resolved }