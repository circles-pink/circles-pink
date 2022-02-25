module Wallet.PrivateKey
  ( Mnemonic
  , PrivateKey
  , genPrivateKey
  , getWords
  , keyToMnemonic
  , mnemonicToKey
  , toEntropy
  , toString
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Data.String (Pattern(..))
import Data.String as S

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type Entropy
  = String

newtype PrivateKey
  = PrivateKey Entropy

newtype Mnemonic
  = Mnemonic (Array String)

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
getWords :: Mnemonic -> Array String
getWords (Mnemonic ws) = ws

toString :: PrivateKey -> String
toString (PrivateKey k) = "0x" <> k

toEntropy :: PrivateKey -> Entropy
toEntropy (PrivateKey e) = e

genPrivateKey :: Aff PrivateKey
genPrivateKey = liftEffect genPrivateKeyImpl

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

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
separator :: String
separator = " "

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import genPrivateKeyImpl :: Effect PrivateKey

foreign import entropyToMnemonicImpl :: String -> String

foreign import mnemonicToEntropyImpl :: String -> String
