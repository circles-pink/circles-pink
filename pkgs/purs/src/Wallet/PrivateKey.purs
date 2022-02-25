module Wallet.PrivateKey
  ( PrivateKey
  , genPrivateKey
  , keyToMnemonic
  , mnemonicToKey
  ) where

import Prelude
import Crypto.Subtle.Constants.EC (ecdsa, p256)
import Crypto.Subtle.Key.Generate (ec, generateKeyPair)
import Crypto.Subtle.Key.Types (CryptoKey, CryptoKeyPair(..), allUsages, exportKey, raw)
import Data.ArrayBuffer.BIP39 (entropyToMnemonic, mnemonicToEntropy)
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Undefined (undefined)

newtype PrivateKey
  = PrivateKey Uint8Array

keyToMnemonic :: PrivateKey -> Maybe String
keyToMnemonic (PrivateKey k) = entropyToMnemonic k

mnemonicToKey :: String -> PrivateKey
mnemonicToKey = undefined

genPrivateKey :: Aff PrivateKey
genPrivateKey =
  generateKeyPair (ec ecdsa p256) true allUsages
    >>= \(CryptoKeyPair { privateKey }) -> cryptoKeyToPrivateKey privateKey

cryptoKeyToPrivateKey :: CryptoKey -> Aff PrivateKey
cryptoKeyToPrivateKey ck =
  exportKey raw ck
    <#> (PrivateKey <<< arrayBufferToUint8Array)

arrayBufferToUint8Array :: ArrayBuffer -> Uint8Array
arrayBufferToUint8Array = ?a
