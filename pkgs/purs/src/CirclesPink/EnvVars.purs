module CirclesPink.EnvVars where

import Prelude
import CirclesPink.URI (URI)
import Control.Monad.Except (ExceptT(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Core.Signatures as Web3
import Node.Process (getEnv)
import Type.Proxy (Proxy(..))
import TypedEnv (class ParseValue, EnvError, Resolved, Variable)
import TypedEnv (fromEnv) as TypedEnv

type Config :: forall k. (Symbol -> Type -> k) -> Row k
type Config f
  = ( gardenApi :: f "GARDEN_API" URI
    , gardenApiUsers :: f "GARDEN_API_USERS" String
    , gardenGraphApi :: f "GARDEN_GRAPH_API" String
    , gardenSubgraphName :: f "GARDEN_SUBGRAPH_NAME" String
    , gardenRelay :: f "GARDEN_RELAY" String
    , gardenHubAdress :: f "GARDEN_HUB_ADDRESS" Address
    , gardenProxyFactoryAdress :: f "GARDEN_PROXY_FACTORY_ADRESS" String
    , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" String
    , gardenEthereumNodeWs :: f "GARDEN_ETHEREUM_NODE_WS" String
    )

--------------------------------------------------------------------------------
-- Newtype Wrappers / Address
--------------------------------------------------------------------------------
newtype Address
  = Address Web3.Address

instance parseValueAddress :: ParseValue Address where
  parseValue x = mkHexString x >>= mkAddress <#> Address

--------------------------------------------------------------------------------
type ResolvedConfig
  = Record (Config Resolved)

parse :: ExceptT EnvError Aff ResolvedConfig
parse =
  TypedEnv.fromEnv (Proxy :: _ (Config Variable))
    <$> getEnv
    # liftEffect
    # ExceptT
