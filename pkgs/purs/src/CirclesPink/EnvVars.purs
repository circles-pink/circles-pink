module CirclesPink.EnvVars where

import Prelude
import CirclesPink.URI (URI)
import Control.Monad.Except (ExceptT(..))
import Data.Either (Either)
import Effect (Effect)
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
    , gardenApiUsers :: f "GARDEN_API_USERS" URI
    , gardenGraphApi :: f "GARDEN_GRAPH_API" URI
    , gardenSubgraphName :: f "GARDEN_SUBGRAPH_NAME" String
    , gardenRelay :: f "GARDEN_RELAY" URI
    , gardenHubAdress :: f "GARDEN_HUB_ADDRESS" Address
    , gardenProxyFactoryAdress :: f "GARDEN_PROXY_FACTORY_ADRESS" Address
    , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" Address
    , gardenEthereumNodeWs :: f "GARDEN_ETHEREUM_NODE_WS" URI
    )

--------------------------------------------------------------------------------
-- Newtype Wrappers / Address
--------------------------------------------------------------------------------
newtype Address
  = Address Web3.Address

instance parseValueAddress :: ParseValue Address where
  parseValue x = mkHexString x >>= mkAddress <#> Address

derive newtype instance showAddress :: Show Address

--------------------------------------------------------------------------------
type ResolvedConfig
  = Record (Config Resolved)

getParsedEnv :: Effect (Either EnvError ResolvedConfig)
getParsedEnv =
  TypedEnv.fromEnv (Proxy :: _ (Config Variable))
    <$> getEnv
