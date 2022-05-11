module Network.Ethereum.Core.Signatures.Extra
  ( ChecksumAddress
  , unChecksumAddress
  , unsafeFromString
  ) where

import Prelude
import Convertable (class Convertible, convert)
import Data.Maybe (fromJust)
import Network.Ethereum.Core.HexString (HexString, mkHexString, unHex)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import TypedEnv (class ParseValue)
import Undefined (undefined)
import Wallet.PrivateKey as C
import Web3.Bindings (web3)

newtype ChecksumAddress
  = ChecksumAddress HexString

unsafeFromString :: Partial => String -> ChecksumAddress
unsafeFromString x = mkHexString x # fromJust # ChecksumAddress

unChecksumAddress :: ChecksumAddress -> HexString
unChecksumAddress (ChecksumAddress h) = h

instance parseValueAddress :: ParseValue ChecksumAddress where
  parseValue x = mkHexString x >>= mkAddress <#> convert

--------------------------------------------------------------------------------
instance convertible_ChecksumAddress_CAddress :: Convertible ChecksumAddress C.Address where
  convert p =
    unsafePartial
      (p # unChecksumAddress # unHex # C.unsafeAddrFromString)

instance convertible_ChecksumAddress_W3Address :: Convertible ChecksumAddress W3.Address where
  convert p =
    unsafePartial
      (p # unChecksumAddress # W3.mkAddress # fromJust)

instance convertible_W3Address_ChecksumAddress :: Convertible W3.Address ChecksumAddress where
  convert p =
    unsafePartial
      ( p
          # W3.unAddress
          # unHex
          # web3.utils.toChecksumAddress
          # unsafeFromString
      )

instance convertible_CAddress_ChecksumAddress :: Convertible C.Address ChecksumAddress where
  convert p =
    unsafePartial
      (p # C.addrToString # web3.utils.toChecksumAddress # unsafeFromString)
