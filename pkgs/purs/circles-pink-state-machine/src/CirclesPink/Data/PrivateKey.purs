module CirclesPink.Data.PrivateKey
  ( PrivateKey(..)
  ) where

import Prelude

import Control.Error.Util (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)

newtype PrivateKey = PrivateKey W3.PrivateKey

derive instance privateKey :: Newtype PrivateKey _

derive newtype instance showPrivateKey :: Show PrivateKey

derive newtype instance eqPrivateKey :: Eq PrivateKey

instance decodeJsonPrivateKey :: DecodeJson PrivateKey where
  decodeJson j = do
    hex <- decodeJson j
    mkPrivateKey hex # note (TypeMismatch "Not a valid Private Key") <#> wrap

instance encodeJsonPrivateKey :: EncodeJson PrivateKey where
  encodeJson (PrivateKey s) = encodeJson $ show s

genPrivateKey :: Aff PrivateKey
genPrivateKey = liftEffect genPrivateKeyImpl <#> unsafeMkPrivateKey

unsafeMkPrivateKey :: String -> PrivateKey
unsafeMkPrivateKey s = wrap $ unsafePartial
  $ fromJust
  $ mkPrivateKey
  $ unsafeMkHexString s

foreign import genPrivateKeyImpl :: Effect String

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s
