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
  , sampleKey
  , sampleMnemonic
  ) where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.BigInt (BigInt)
import Data.BigInt as B
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import CirclesPink.Data.PrivateKey (PrivateKey(..))
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type Entropy = String

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------










keyToMnemonic :: PrivateKey -> Mnemonic
keyToMnemonic k = toEntropy k
  # entropyToMnemonicImpl
  # S.split (Pattern separator)
  # Mnemonic

mnemonicToKey :: Mnemonic -> PrivateKey
mnemonicToKey (Mnemonic ws) = unsafePartial result
  where
  result :: Partial => PrivateKey
  result = S.joinWith separator ws
    # mnemonicToEntropyImpl Nothing Just
    # fromJust
    # unsafeMkPrivateKey

addressToNonce :: W3.Address -> Nonce
addressToNonce addr = Nonce $ addressToNonceImpl $ show addr

sampleKey :: PrivateKey
sampleKey =
  unsafeMkPrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"


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

foreign import entropyToMnemonicImpl :: String -> String


foreign import privKeyToAddressImpl :: String -> String

foreign import addressToNonceImpl :: String -> BigInt

foreign import isPrivateKeyImpl :: String -> Boolean

