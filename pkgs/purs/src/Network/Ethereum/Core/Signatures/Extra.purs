module Network.Ethereum.Core.Signatures.Extra
  ( ChecksumAddress
  --, unChecksumAddress
  , unsafeFromString
  ) where

import Prelude
import Convertable (class Convertible, convert)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Network.Ethereum.Core.HexString (HexString, mkHexString, unHex)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import TypedEnv (class ParseValue)
import Wallet.PrivateKey as C
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
instance convertible_ChecksumAddress_CAddress :: Convertible ChecksumAddress C.Address where
  convert (ChecksumAddress a) =
    unsafePartial
      (W3.unAddress a # unHex # web3static.utils.toChecksumAddress # C.unsafeAddrFromString)

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

instance convertible_CAddress_ChecksumAddress :: Convertible C.Address ChecksumAddress where
  convert p =
    unsafePartial
      (p # C.addrToString # unsafeFromString)
