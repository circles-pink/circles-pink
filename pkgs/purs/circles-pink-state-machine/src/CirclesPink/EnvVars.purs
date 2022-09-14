module CirclesPink.EnvVars
  ( Config
  , EnvVars(..)
  , getParsedEnv
  ) where

import CirclesPink.Prelude
import CirclesPink.Garden.EnvControlAff as E
import CirclesPink.URI (URI)
import CirclesPink.URI as U
import Convertable (class Convertible)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Node.Process (getEnv)
import Record.Extra.CirclesPink (zipRecord)
import Type.Proxy (Proxy(..))
import TypedEnv (EnvError, Resolved, Variable)
import TypedEnv (fromEnv) as TypedEnv

type Config :: forall k. (Symbol -> Type -> k) -> Row k
type Config f =
  ( gardenApi :: f "GARDEN_API" URI
  , gardenApiUsers :: f "GARDEN_API_USERS" URI
  , gardenGraphApi :: f "GARDEN_GRAPH_API" URI
  , gardenSubgraphName :: f "GARDEN_SUBGRAPH_NAME" String
  , gardenRelay :: f "GARDEN_RELAY" URI
  , gardenHubAddress :: f "GARDEN_HUB_ADDRESS" ChecksumAddress
  , gardenProxyFactoryAddress :: f "GARDEN_PROXY_FACTORY_ADRESS" ChecksumAddress
  , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" ChecksumAddress
  , gardenEthereumNodeWebSocket :: f "GARDEN_ETHEREUM_NODE_WS" URI
  , voucherServerHost :: f "VOUCHER_SERVER_HOST" URI
  , isDev :: f "IS_DEV" Boolean
  )

--------------------------------------------------------------------------------
newtype EnvVars = EnvVars (Record (Config Resolved))

derive instance newtypeEnvVars :: Newtype EnvVars _

instance convertibleEnvVars :: Convertible EnvVars E.EnvVars where
  convert (EnvVars env) =
    zipRecord
      { gardenApi: U.print
      , gardenApiUsers: U.print
      , gardenGraphApi: U.print
      , gardenSubgraphName: identity :: String -> _
      , gardenRelay: U.print
      , gardenHubAddress: show :: ChecksumAddress -> _
      , gardenProxyFactoryAddress: show :: ChecksumAddress -> _
      , gardenSafeMasterAddress: show :: ChecksumAddress -> _
      , gardenEthereumNodeWebSocket: U.print
      , voucherServerHost: U.print
      , isDev: identity :: Boolean -> _
      }
      env
      # E.EnvVars

--------------------------------------------------------------------------------
getParsedEnv :: Effect (Either EnvError EnvVars)
getParsedEnv =
  getEnv
    <#> (TypedEnv.fromEnv (Proxy :: _ (Config Variable)) >>> map EnvVars)

--------------------------------------------------------------------------------
