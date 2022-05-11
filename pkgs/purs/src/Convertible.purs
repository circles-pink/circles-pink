module Convertable where

import Prelude
import Data.Maybe (fromJust)
import Network.Ethereum.Core.HexString (mkHexString, unHex)
import Network.Ethereum.Core.Signatures as W3
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress, unChecksumAddress, unsafeFromString)
import Partial.Unsafe (unsafePartial)
import Wallet.PrivateKey as C
import Web3.Bindings (web3)

class Convertible a b where
  convert :: a -> b

--------------------------------------------------------------------------------
instance convertible_CPrivateKey_W3PrivateKey :: Convertible C.PrivateKey W3.PrivateKey where
  convert p = unsafePartial (C.toEntropy p # mkHexString >>= W3.mkPrivateKey # fromJust)

instance convertible_W3PrivateKey_CPrivateKey :: Convertible W3.PrivateKey C.PrivateKey where
  convert p = unsafePartial (W3.unPrivateKey p # unHex # C.unsafeMkPrivateKey)

--------------------------------------------------------------------------------
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

instance convertible_ChecksumAddress_W3Address :: Convertible ChecksumAddress W3.Address where
  convert p =
    unsafePartial
      (p # unChecksumAddress # W3.mkAddress # fromJust)

instance convertible_ChecksumAddress_CAddress :: Convertible ChecksumAddress C.Address where
  convert p =
    unsafePartial
      (p # unChecksumAddress # unHex # C.unsafeAddrFromString)

instance convertible_W3Address_CAddress :: Convertible W3.Address C.Address where
  convert p =
    unsafePartial
      (p # W3.unAddress # unHex # C.unsafeAddrFromString)

instance convertible_CAddress_W3Address :: Convertible C.Address W3.Address where
  convert p =
    unsafePartial
      (p # C.addrToString # mkHexString >>= W3.mkAddress # fromJust)
