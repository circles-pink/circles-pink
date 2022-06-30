module CirclesPink.Data.Address
  ( Address(..)
  , module Exp
  , sampleAddress
  , sampleSafeAddress
  )
  where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, wrap)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress) as Exp
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)

newtype Address = Address W3.Address

derive instance newtype_ :: Newtype Address _

derive newtype instance show :: Show Address

derive newtype instance eq :: Eq Address

derive newtype instance ord :: Ord Address

derive newtype instance decodeJson :: DecodeJson Address

derive newtype instance encodeJson :: EncodeJson Address


sampleAddress :: Address
sampleAddress = unsafeMkAddress "fb7dc4d8f841af32d777e698d6c71409e85955d9"

sampleSafeAddress :: Address
sampleSafeAddress = unsafeMkAddress "984501180D63335928eA7fb59c17d33e0398Ed39"


unsafeMkAddress :: String -> Address
unsafeMkAddress s = unsafePartial
  $ wrap
  $ fromJust
  $ W3.mkAddress
  $ unsafeMkHexString s

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s
