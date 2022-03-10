module Wallet.PrivateKey
  ( Address
  , Mnemonic
  , Nonce
  , PrivateKey
  , addrToString
  , addressToNonce
  , genPrivateKey
  , getWords
  , keyToMnemonic
  , mnemonicToKey
  , nonceToBigInt
  , nonceToString
  , privKeyToAddress
  , sampleAddress
  , sampleKey
  , toEntropy
  , toString
  , unsafeAddrFromString
  , zeroKey
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.BigInt as B
import Data.String (Pattern(..))
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type Entropy
  = String

newtype Address
  = Address String

newtype Nonce
  = Nonce BigInt

newtype PrivateKey
  = PrivateKey Entropy

derive instance privateKeyEq :: Eq PrivateKey

instance showPrivateKey :: Show PrivateKey where
  show _ = "***private_key***"

newtype Mnemonic
  = Mnemonic (Array String)

derive instance mnemonicEq :: Eq Mnemonic

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
getWords :: Mnemonic -> Array String
getWords (Mnemonic ws) = ws

toString :: PrivateKey -> String
toString (PrivateKey k) = "0x" <> k

addrToString :: Address -> String
addrToString (Address a) = a

unsafeAddrFromString :: String -> Address
unsafeAddrFromString s = Address s

nonceToString :: Nonce -> String
nonceToString (Nonce n) = B.toString n

nonceToBigInt :: Nonce -> BigInt
nonceToBigInt (Nonce n) = n

toEntropy :: PrivateKey -> Entropy
toEntropy (PrivateKey e) = e

genPrivateKey :: Aff PrivateKey
genPrivateKey = liftEffect genPrivateKeyImpl <#> (PrivateKey <<< S.drop 2)

keyToMnemonic :: PrivateKey -> Mnemonic
keyToMnemonic k =
  toEntropy k
    # entropyToMnemonicImpl
    # S.split (Pattern separator)
    # Mnemonic

mnemonicToKey :: Mnemonic -> PrivateKey
mnemonicToKey (Mnemonic ws) =
  S.joinWith separator ws
    # mnemonicToEntropyImpl
    # PrivateKey

zeroKey :: PrivateKey
zeroKey = PrivateKey "0000000000000000000000000000000000000000000000000000000000000000"

sampleKey :: PrivateKey
sampleKey = PrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"

sampleAddress :: Address
sampleAddress = Address "0xfb7dc4d8f841af32d777e698d6c71409e85955d9"

privKeyToAddress :: PrivateKey -> Address
privKeyToAddress pk = Address $ privKeyToAddressImpl $ toString pk

addressToNonce :: Address -> Nonce
addressToNonce a = Nonce $ addressToNonceImpl $ addrToString a

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
separator :: String
separator = " "

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import genPrivateKeyImpl :: Effect String

foreign import entropyToMnemonicImpl :: String -> String

foreign import mnemonicToEntropyImpl :: String -> String

foreign import privKeyToAddressImpl :: String -> String

foreign import addressToNonceImpl :: String -> BigInt
