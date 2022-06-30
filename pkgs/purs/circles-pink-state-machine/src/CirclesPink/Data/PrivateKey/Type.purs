module CirclesPink.Data.PrivateKey.Type
  ( PrivateKey
  ) where

import Prelude

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
