module CirclesPink.EnvVars
  ( Address(..)
  , Config
  , EnvVars(..)
  , getParsedEnv
  ) where

import Prelude
import CirclesPink.Garden.Env as E
import CirclesPink.URI (URI)
import CirclesPink.URI as U
import Convertable (class Convertible)
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Newtype.Extra ((-|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Undefined (undefined)
import Debug (spy)
import Effect (Effect)
import Heterogeneous.Mapping (hmapWithIndex)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Core.Signatures as Web3
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Node.Process (getEnv)
import Record.Extra.CirclesPink (zipRecord)
import Stadium.Type.Tuple (Tuple)
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
    , gardenHubAddress :: f "GARDEN_HUB_ADDRESS" Address
    , gardenProxyFactoryAddress :: f "GARDEN_PROXY_FACTORY_ADRESS" Address
    , gardenSafeMasterAddress :: f "GARDEN_SAFE_MASTER_ADDRESS" Address
    , gardenEthereumNodeWebSocket :: f "GARDEN_ETHEREUM_NODE_WS" URI
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
newtype EnvVars
  = EnvVars (Record (Config Resolved))

derive instance newtypeEnvVars :: Newtype EnvVars _

instance convertibleEnvVars :: Convertible EnvVars E.EnvVars where
  convert (EnvVars env) =
    zipRecord
      { gardenApi: U.print
      , gardenApiUsers: U.print
      , gardenGraphApi: U.print
      , gardenSubgraphName: identity :: String -> _
      , gardenRelay: U.print
      , gardenHubAddress: show :: Address -> _
      , gardenProxyFactoryAddress: show :: Address -> _
      , gardenSafeMasterAddress: show :: Address -> _
      , gardenEthereumNodeWebSocket: U.print
      }
      (spy "e1" env)
      # E.EnvVars
      # spy "e"

--------------------------------------------------------------------------------
getParsedEnv :: Effect (Either EnvError EnvVars)
getParsedEnv =
  getEnv
    <#> (TypedEnv.fromEnv (Proxy :: _ (Config Variable)) >>> map EnvVars)

--------------------------------------------------------------------------------
