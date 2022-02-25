module Wallet.PrivateKey
  ( genPrivateKey
  , keyToMnemonic
  , mnemonicToKey
  , PrivateKey
  , tests
  ) where

import Prelude
import Crypto.Subtle.Constants.EC (ecdsa, p256)
import Crypto.Subtle.Key.Generate (ec, generateKeyPair)
import Crypto.Subtle.Key.Types (CryptoKey, CryptoKeyPair(..), allUsages, exportKey, raw)
import Data.ArrayBuffer.BIP39 (entropyToMnemonic, mnemonicToEntropy)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.Unit as T
import Test.Unit.Assert as A

newtype PrivateKey
  = PrivateKey Uint8Array

keyToMnemonic :: PrivateKey -> String
keyToMnemonic (PrivateKey k) =
  unsafePartial case entropyToMnemonic k of
    Just x -> x

mnemonicToKey :: String -> PrivateKey
mnemonicToKey mn =
  unsafePartial case mnemonicToEntropy mn of
    Just x -> PrivateKey x

genPrivateKey :: Aff PrivateKey
genPrivateKey =
  generateKeyPair (ec ecdsa p256) true allUsages
    >>= \(CryptoKeyPair { privateKey }) -> cryptoKeyToPrivateKey privateKey

cryptoKeyToPrivateKey :: CryptoKey -> Aff PrivateKey
cryptoKeyToPrivateKey ck =
  exportKey raw ck
    >>= (liftEffect <<< whole)
    <#> PrivateKey

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
tests :: T.TestSuite
tests =
  T.suite "PrivateKey" do
    T.test "graph1" do
      A.equal 1 1
