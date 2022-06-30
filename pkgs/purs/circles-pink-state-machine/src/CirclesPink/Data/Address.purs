module CirclesPink.Data.Address
  ( Address(..)
  , module Exp
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)
import Network.Ethereum.Core.Signatures (mkAddress) as Exp
import Network.Ethereum.Core.Signatures as W3

newtype Address = Address W3.Address

derive instance newtype_ :: Newtype Address _

derive newtype instance show :: Show Address

derive newtype instance eq :: Eq Address

derive newtype instance ord :: Ord Address

derive newtype instance decodeJson :: DecodeJson Address

derive newtype instance encodeJson :: EncodeJson Address
