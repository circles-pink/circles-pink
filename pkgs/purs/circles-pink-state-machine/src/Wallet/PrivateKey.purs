module Wallet.PrivateKey
  ( Address(..)
  , Mnemonic
  , Nonce
  , PrivateKey
  , addrToString
  , addressToNonce
  , genPrivateKey
  , getMnemonicFromString
  , getWords
  , isPrivateKey
  , keyToMnemonic
  , mnemonicToKey
  , nonceToBigInt
  , nonceToString
  , privKeyToAddress
  , sampleAddress
  , sampleKey
  , sampleMnemonic
  , sampleSafeAddress
  , toEntropy
  , toString
  , unsafeAddrFromString
  , unsafeMkPrivateKey
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.BigInt (BigInt)
import Data.BigInt as B
import Data.Either (Either(..))
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type Entropy = String

--------------------------------------------------------------------------------
newtype Address = Address String

derive newtype instance showAddress :: Show Address

derive newtype instance mnemonicAddress :: EncodeJson Address

derive instance eqAddress :: Eq Address

--------------------------------------------------------------------------------
newtype Nonce = Nonce BigInt

newtype PrivateKey = PrivateKey Entropy

derive instance privateKeyEq :: Eq PrivateKey

derive newtype instance showPrivateKey :: Show PrivateKey

--------------------------------------------------------------------------------
newtype Mnemonic = Mnemonic (Array String)

derive instance mnemonicEq :: Eq Mnemonic

derive newtype instance mnemonicEncodeJson :: EncodeJson Mnemonic

instance mnemonicShow :: Show Mnemonic where
  show (Mnemonic xs) = joinWith " " xs -- TODO: Remove!

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
getWords :: Mnemonic -> Array String
getWords (Mnemonic ws) = ws

unsafeMkPrivateKey :: Partial => String -> PrivateKey
unsafeMkPrivateKey hexString = PrivateKey $ S.drop 2 $ hexString

getMnemonicFromString :: String -> Mnemonic
getMnemonicFromString s = Mnemonic $ R.split (unsafeRegex " +" noFlags) $ S.trim s

toString :: PrivateKey -> String
toString (PrivateKey k) = "0x" <> k

addrToString :: Address -> String
addrToString (Address a) = a

unsafeAddrFromString :: Partial => String -> Address
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

sampleKey :: PrivateKey
sampleKey = PrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"

sampleAddress :: Address
sampleAddress = Address "0xfb7dc4d8f841af32d777e698d6c71409e85955d9"

sampleSafeAddress :: Address
sampleSafeAddress = Address "0x984501180D63335928eA7fb59c17d33e0398Ed39"

sampleMnemonic :: Mnemonic
sampleMnemonic = Mnemonic [ "gym", "onion", "turkey", "slice", "blue", "random", "goat", "live", "grit", "educate", "slam", "alone", "enroll", "print", "need", "certain", "stumble", "addict", "drive", "accident", "iron", "provide", "major", "next" ]

isPrivateKey :: String -> Boolean
isPrivateKey s = isPrivateKeyImpl s

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

foreign import isPrivateKeyImpl :: String -> Boolean

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance decodeJsonPrivateKey :: DecodeJson PrivateKey where
  decodeJson j = do
    result <- decodeJson j
    if (isPrivateKey result) then
      Right $ PrivateKey result
    else
      Left $ TypeMismatch "Not a valid Private Key"

instance encodeJsonPrivateKey :: EncodeJson PrivateKey where
  encodeJson (PrivateKey s) = encodeJson s
