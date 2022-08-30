module VoucherServer.EnvVars
  ( AppEnvVars(..)
  , It
  , MkAppEnvVars
  , MkAppEnvVarsMandatory
  , MkAppEnvVarsOptional
  , MkOptional
  , parseEnvVars
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds(..))
import Foreign.Object (Object)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (PrivateKey, mkPrivateKey)
import Network.Ethereum.Core.Signatures as C
import Partial.Unsafe (unsafePartial)
import Record as R
import Safe.Coerce (coerce)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Type.Proxy (Proxy(..))
import TypedEnv (class ParseValue, EnvError, Variable, Resolved, fromEnv, parseValue)
import VoucherServer.Specs.Xbge (Address)
import Type.Row (type (+))

--------------------------------------------------------------------------------
-- EnvVars
--------------------------------------------------------------------------------

defaults :: { | MkAppEnvVarsOptional Resolved It () }
defaults =
  { port: 4000
  , syncInterval: Seconds 5.0
  }

type MkAppEnvVarsMandatory :: DefEnvVars
type MkAppEnvVarsMandatory def wrap r =
  ( gardenApi ::
      def "GARDEN_API" String
  , gardenApiUsers ::
      def "GARDEN_API_USERS" String
  , gardenGraphApi ::
      def "GARDEN_GRAPH_API" String
  , gardenSubgraphName ::
      def "GARDEN_SUBGRAPH_NAME" String
  , gardenRelay ::
      def "GARDEN_RELAY" String
  , gardenHubAddress ::
      def "GARDEN_HUB_ADDRESS" String
  , gardenProxyFactoryAddress ::
      def "GARDEN_PROXY_FACTORY_ADRESS" String
  , gardenSafeMasterAddress ::
      def "GARDEN_SAFE_MASTER_ADDRESS" String
  , gardenEthereumNodeWebSocket ::
      def "GARDEN_ETHEREUM_NODE_WS" String
  , voucherCodeSecret ::
      def "VOUCHER_CODE_SECRET" String
  , xbgeAuthSecret ::
      def "XBGE_AUTH_SECRET" String
  , xbgeEndpoint ::
      def "XBGE_ENDPOINT" String
  , xbgeSafeAddress ::
      def "XBGE_SAFE_ADDRESS" Address
  , xbgeKey ::
      def "XBGE_KEY" (wrap C.PrivateKey)
  , voucherServerBasicAuth ::
      def "VOUCHER_SERVER_BASIC_AUTH" String
  | r
  )

type MkAppEnvVarsOptional :: DefEnvVars
type MkAppEnvVarsOptional def wrap r =
  ( port ::
      def "PORT" Int
  , syncInterval ::
      def "SYNC_INTERVAL" (wrap Seconds)
  | r
  )

--------------------------------------------------------------------------------

type MkAppEnvVars ::  (Symbol -> Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Row Type
type MkAppEnvVars f m w =
  MkAppEnvVarsMandatory f w + MkAppEnvVarsOptional (MkOptional f m) w + ()

type DefEnvVars = (Symbol -> Type -> Type) -> (Type -> Type) -> Row Type -> Row Type

type MkOptional ::  (Symbol -> Type -> Type) -> (Type -> Type) -> Symbol -> Type -> Type
type MkOptional f w s t = f s (w t)

type AppEnvVarsSpec = MkAppEnvVars Variable Maybe WrapEnv

newtype AppEnvVars = AppEnvVars { | MkAppEnvVars Resolved It It }

type It :: forall k. k -> k
type It ty = ty

parseEnvVars :: Object String -> Either EnvError AppEnvVars
parseEnvVars obj = do
  envVars' <- fromEnv (Proxy :: _ AppEnvVarsSpec) obj
  let (envVars'' :: { | MkAppEnvVars Resolved Maybe It }) = coerce envVars'
  let envVars = R.merge defaults envVars''
  pure $ AppEnvVars envVars

--------------------------------------------------------------------------------
-- Newtypes
--------------------------------------------------------------------------------

newtype WrapEnv a = WrapEnv a

derive instance Newtype (WrapEnv a) _

instance Arbitrary (WrapEnv PrivateKey) where
  arbitrary = pure $ WrapEnv sampleKey

instance Arbitrary (WrapEnv Seconds) where
  arbitrary = arbitrary <#> Seconds >>> WrapEnv

instance ParseValue (WrapEnv Seconds) where
  parseValue x = parseValue x <#> Seconds >>> WrapEnv

instance ParseValue (WrapEnv PrivateKey) where
  parseValue x = parsePrivKey x <#> WrapEnv

instance Arbitrary AppEnvVars where
  arbitrary = do
    r :: { | MkAppEnvVars Resolved It WrapEnv } <- arbitrary
    pure $ AppEnvVars (coerce r)

parsePrivKey :: String -> Maybe PrivateKey
parsePrivKey x = mkHexString x >>= mkPrivateKey

sampleKey :: C.PrivateKey
sampleKey =
  unsafePartial
    $ fromJust
    $
      parsePrivKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"

