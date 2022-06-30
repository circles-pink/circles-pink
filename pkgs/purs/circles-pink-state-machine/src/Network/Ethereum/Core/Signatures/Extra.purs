module Network.Ethereum.Core.Signatures.Extra
  ( ChecksumAddress
  --, unChecksumAddress
  , unsafeFromString
  ) where

import Prelude

import CirclesPink.Data.Address (Address)
import Convertable (class Convertible)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Core.Signatures as W3
import TypedEnv (class ParseValue)
import Web3.Bindings (web3static)

newtype ChecksumAddress = ChecksumAddress W3.Address

instance showChecksumAddress :: Show ChecksumAddress where
  show (ChecksumAddress a) = show a # web3static.utils.toChecksumAddress

unsafeFromString :: Partial => String -> ChecksumAddress
unsafeFromString x = mkHexString x >>= W3.mkAddress # fromJust # ChecksumAddress

instance parseValueAddress :: ParseValue ChecksumAddress where
  parseValue x = mkHexString x >>= mkAddress <#> ChecksumAddress

--------------------------------------------------------------------------------

instance convertible_ChecksumAddress_W3Address :: Convertible ChecksumAddress Address where
  convert (ChecksumAddress a) = wrap a

instance convertible_W3Address_ChecksumAddress :: Convertible Address ChecksumAddress where
  convert = unwrap >>> ChecksumAddress
