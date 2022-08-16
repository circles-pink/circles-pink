module VoucherServer.EnvVars
  ( MkAppEnvVars
  , PrivateKey(..)
  , AppEnvVars
  , AppEnvVarsSpec
  ) where

import Prelude

import Data.Maybe (Maybe)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as C
import TypedEnv (class ParseValue, Resolved, Variable)
import VoucherServer.Specs.Xbge (Address)

type MkAppEnvVars :: forall k. (Symbol -> Type -> k) -> Row k
type MkAppEnvVars f =
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
  , xbgeKey :: f "XBGE_KEY" PrivateKey
  )

type AppEnvVarsSpec = MkAppEnvVars Variable

type AppEnvVars = { | MkAppEnvVars Resolved }

--------------------------------------------------------------------------------
-- Newtypes
--------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey C.PrivateKey

-- derive instance newtypePrivateKey :: Newtype PrivateKey

instance parseValuePrivateKey :: ParseValue PrivateKey where
  parseValue x = mkHexString x >>= mkPrivateKey <#> PrivateKey

--------------------------------------------------------------------------------
