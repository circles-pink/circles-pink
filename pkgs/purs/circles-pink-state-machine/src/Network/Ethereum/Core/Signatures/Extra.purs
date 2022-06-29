module Network.Ethereum.Core.Signatures.Extra
  ( ChecksumAddress
  --, unChecksumAddress
  , unsafeFromString
  ) where

import Prelude
import Convertable (class Convertible, convert)
import Data.Maybe (fromJust)
import Network.Ethereum.Core.HexString (mkHexString, unHex)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import TypedEnv (class ParseValue)
import Web3.Bindings (web3static)

newtype ChecksumAddress = ChecksumAddress W3.Address

instance showChecksumAddress :: Show ChecksumAddress where
  show (ChecksumAddress a) = show a # web3static.utils.toChecksumAddress

unsafeFromString :: Partial => String -> ChecksumAddress
unsafeFromString x = mkHexString x >>= W3.mkAddress # fromJust # ChecksumAddress

-- unChecksumAddress :: ChecksumAddress -> HexString
-- unChecksumAddress (ChecksumAddress a) = h
instance parseValueAddress :: ParseValue ChecksumAddress where
  parseValue x = mkHexString x >>= mkAddress <#> convert

--------------------------------------------------------------------------------

instance convertible_ChecksumAddress_W3Address :: Convertible ChecksumAddress W3.Address where
  convert (ChecksumAddress a) = a

instance convertible_W3Address_ChecksumAddress :: Convertible W3.Address ChecksumAddress where
  convert p =
    unsafePartial
      ( p
          # W3.unAddress
          # unHex
          # unsafeFromString
      )

