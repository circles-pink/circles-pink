module Wallet.PrivateKey
  ( Address
  , Mnemonic
  , PrivateKey
  , addrToString
  , genPrivateKey
  , getWords
  , keyToMnemonic
  , mnemonicToKey
  , privKeyToAddress
  , sampleKey
  , toEntropy
  , toString
  , zeroKey
  ) where

import Prelude
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

privKeyToAddress :: PrivateKey -> Address
privKeyToAddress pk = Address $ privKeyToAddressImpl $ toString pk

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
