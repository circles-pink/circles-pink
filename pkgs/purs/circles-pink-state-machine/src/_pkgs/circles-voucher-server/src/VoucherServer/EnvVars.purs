module VoucherServer.EnvVars
  ( AppEnvVars(..)
  , AppEnvVarsSpec
  , MkAppEnvVars
  , PrivateKey(..)
  )
  where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as C
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import TypedEnv (class ParseValue, Resolved, Variable)
import VoucherServer.Specs.Xbge (Address)

--------------------------------------------------------------------------------
-- EnvVars
--------------------------------------------------------------------------------

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
  , voucherServerBasicAuth :: f "VOUCHER_SERVER_BASIC_AUTH" String
  )

type AppEnvVarsSpec = MkAppEnvVars Variable

newtype AppEnvVars = AppEnvVars { | MkAppEnvVars Resolved }

derive newtype instance Arbitrary AppEnvVars

--------------------------------------------------------------------------------
-- Newtypes
--------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey C.PrivateKey

instance arbitraryPrivateKey :: Arbitrary PrivateKey where
  arbitrary = pure sampleKey

-- derive instance newtypePrivateKey :: Newtype PrivateKey

instance parseValuePrivateKey :: ParseValue PrivateKey where
  parseValue = parsePrivKey

parsePrivKey :: String -> Maybe PrivateKey
parsePrivKey x = mkHexString x >>= mkPrivateKey <#> PrivateKey

sampleKey :: PrivateKey
sampleKey =
  unsafePartial
    $ fromJust
    $
      parsePrivKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"
