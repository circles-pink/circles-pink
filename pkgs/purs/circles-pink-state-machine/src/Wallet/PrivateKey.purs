module Wallet.PrivateKey
  ( Mnemonic
  , Nonce
  , addressToNonce
  , genPrivateKey
  , getMnemonicFromString
  , getWords
  , isPrivateKey
  , keyToMnemonic
  , mnemonicToKey
  , nonceToBigInt
  , nonceToString
  , sampleAddress
  , sampleKey
  , sampleMnemonic
  , sampleSafeAddress
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.BigInt (BigInt)
import Data.BigInt as B
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import Data.PrivateKey (PrivateKey(..))
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress, mkPrivateKey)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type Entropy = String

--------------------------------------------------------------------------------
newtype Nonce = Nonce BigInt

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

-- unsafeMkPrivateKey :: Partial => String -> PrivateKey
-- unsafeMkPrivateKey hexString = PrivateKey $ S.drop 2 $ hexString

getMnemonicFromString :: String -> Maybe Mnemonic
getMnemonicFromString s = map (\_ -> Mnemonic $ R.split (unsafeRegex " +" noFlags) $ S.trim s) pk
  where
  pk = mnemonicToEntropyImpl Nothing Just s

nonceToString :: Nonce -> String
nonceToString (Nonce n) = B.toString n

nonceToBigInt :: Nonce -> BigInt
nonceToBigInt (Nonce n) = n

-- toEntropy :: PrivateKey -> Entropy
-- toEntropy (PrivateKey e) = e

genPrivateKey :: Aff PrivateKey
genPrivateKey = liftEffect genPrivateKeyImpl <#> unsafeMkPrivateKey

keyToMnemonic :: PrivateKey -> Mnemonic
keyToMnemonic k = todo

-- toEntropy k
--   # entropyToMnemonicImpl
--   # S.split (Pattern separator)
--   # Mnemonic

mnemonicToKey :: Mnemonic -> PrivateKey
mnemonicToKey (Mnemonic ws) = todo

-- unsafePartial result
-- where
-- result :: Partial => PrivateKey
-- result = S.joinWith separator ws
--   # mnemonicToEntropyImpl Nothing Just
--   # fromJust
--   # PrivateKey

addressToNonce :: W3.Address -> Nonce
addressToNonce addr = Nonce $ addressToNonceImpl $ show addr

sampleKey :: PrivateKey
sampleKey =
  unsafeMkPrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"

sampleAddress :: W3.Address
sampleAddress = unsafeMkAddress "fb7dc4d8f841af32d777e698d6c71409e85955d9"

sampleSafeAddress :: W3.Address
sampleSafeAddress = unsafeMkAddress "984501180D63335928eA7fb59c17d33e0398Ed39"

unsafeMkPrivateKey :: String -> PrivateKey
unsafeMkPrivateKey s = wrap $ unsafePartial
  $ fromJust
  $ mkPrivateKey
  $ unsafeMkHexString s

unsafeMkAddress :: String -> W3.Address
unsafeMkAddress s = unsafePartial
  $ fromJust
  $ mkAddress
  $ unsafeMkHexString s

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s

sampleMnemonic :: Mnemonic
sampleMnemonic = Mnemonic [ "gym", "onion", "turkey", "slice", "blue", "random", "goat", "live", "grit", "educate", "slam", "alone", "enroll", "print", "need", "certain", "stumble", "addict", "drive", "accident", "iron", "provide", "major", "next" ]

isPrivateKey :: String -> Boolean
isPrivateKey s = isPrivateKeyImpl s

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

foreign import mnemonicToEntropyImpl :: Maybe String -> (String -> Maybe String) -> String -> Maybe String

foreign import privKeyToAddressImpl :: String -> String

foreign import addressToNonceImpl :: String -> BigInt

foreign import isPrivateKeyImpl :: String -> Boolean

