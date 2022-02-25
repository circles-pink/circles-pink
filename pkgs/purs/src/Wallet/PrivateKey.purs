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

newtype PrivateKey
  = PrivateKey Entropy

derive instance privateKeyEq :: Eq PrivateKey

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
