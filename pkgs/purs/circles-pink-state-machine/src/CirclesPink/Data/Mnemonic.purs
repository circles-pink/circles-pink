module CirclesPink.Data.Mnemonic
  ( Mnemonic
  , getMnemonicFromString
  , getWords
  )
  where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

newtype Mnemonic = Mnemonic (Array String)

derive instance mnemonicEq :: Eq Mnemonic

derive newtype instance mnemonicEncodeJson :: EncodeJson Mnemonic

instance mnemonicShow :: Show Mnemonic where
  show (Mnemonic xs) = joinWith " " xs -- TODO: Remove!


getWords :: Mnemonic -> Array String
getWords (Mnemonic ws) = ws

getMnemonicFromString :: String -> Maybe Mnemonic
getMnemonicFromString s = map (\_ -> Mnemonic $ R.split (unsafeRegex " +" noFlags) $ S.trim s) pk
  where
  pk = mnemonicToEntropyImpl Nothing Just s


foreign import mnemonicToEntropyImpl :: Maybe String -> (String -> Maybe String) -> String -> Maybe String
