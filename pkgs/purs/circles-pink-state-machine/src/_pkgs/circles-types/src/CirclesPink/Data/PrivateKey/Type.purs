module CirclesPink.Data.PrivateKey.Type
  ( PrivateKey(..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (note)
import Data.Newtype (class Newtype, wrap)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as W3

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
