module CirclesPink.Data.Mnemonic
  ( Mnemonic
  , getMnemonicFromString
  , getWords
  , keyToMnemonic
  , sampleMnemonic
  ) where

import CirclesPink.Prelude

import CirclesPink.Data.PrivateKey.Type (PrivateKey)
import Data.Argonaut (class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

moduleName :: String
moduleName = "CirclesPink.Data.Mnemonic"

newtype Mnemonic = Mnemonic (Array String)

derive instance Eq Mnemonic

derive newtype instance EncodeJson Mnemonic

instance Show Mnemonic where
  show (Mnemonic xs) = joinWith " " xs -- TODO: Remove!

instance ToPursNominal Mnemonic where
  toPursNominal _ = PursNominal moduleName "Mnemonic"

instance ToTsType Mnemonic where
  toTsType = typeRefToTsType' []

instance ToTsDef Mnemonic where
  toTsDef = opaqueToTsDef' []

instance ToPursType Mnemonic where
  toPursType = defaultToPursType' []

getWords :: Mnemonic -> Array String
getWords (Mnemonic ws) = ws

getMnemonicFromString :: String -> Maybe Mnemonic
getMnemonicFromString s = map (\_ -> Mnemonic $ R.split (unsafeRegex " +" noFlags) $ S.trim s) pk
  where
  pk = mnemonicToEntropyImpl Nothing Just s

separator :: String
separator = " "

keyToMnemonic :: PrivateKey -> Mnemonic
keyToMnemonic k = show k
  # entropyToMnemonicImpl
  # S.split (Pattern separator)
  # Mnemonic

sampleMnemonic :: Mnemonic
sampleMnemonic = Mnemonic [ "gym", "onion", "turkey", "slice", "blue", "random", "goat", "live", "grit", "educate", "slam", "alone", "enroll", "print", "need", "certain", "stumble", "addict", "drive", "accident", "iron", "provide", "major", "next" ]

foreign import mnemonicToEntropyImpl :: Maybe String -> (String -> Maybe String) -> String -> Maybe String

foreign import entropyToMnemonicImpl :: String -> String