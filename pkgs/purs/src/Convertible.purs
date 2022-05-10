module Convertable where

import Prelude
import Data.Maybe (fromJust)
import Network.Ethereum.Core.HexString (mkHexString, unHex)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import Wallet.PrivateKey as W

class Convertible a b where
  convert :: a -> b

instance convertiblePrivateKeyFrom :: Convertible W.PrivateKey W3.PrivateKey where
  convert p = unsafePartial (W.toEntropy p # mkHexString >>= mkPrivateKey # fromJust)

instance convertiblePrivateKeyTo :: Convertible W3.PrivateKey W.PrivateKey where
  convert p = unsafePartial (W3.unPrivateKey p # unHex # W.unsafeMkPrivateKey)
