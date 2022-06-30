module CirclesPink.Data.PrivateKey
  ( PrivateKey(..)
  , mnemonicToKey
  , sampleKey
  , module Exp
  ) where

import Prelude

import CirclesPink.Data.Mnemonic (Mnemonic)
import Control.Error.Util (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import CirclesPink.Data.PrivateKey.Type as Exp

genPrivateKey :: Aff PrivateKey
genPrivateKey = liftEffect genPrivateKeyImpl <#> unsafeMkPrivateKey

unsafeMkPrivateKey :: String -> PrivateKey
unsafeMkPrivateKey s = wrap $ unsafePartial
  $ fromJust
  $ mkPrivateKey
  $ unsafeMkHexString s

foreign import genPrivateKeyImpl :: Effect String

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s

mnemonicToKey :: Mnemonic -> PrivateKey
mnemonicToKey (Mnemonic ws) = unsafePartial result
  where
  result :: Partial => PrivateKey
  result = S.joinWith separator ws
    # mnemonicToEntropyImpl Nothing Just
    # fromJust
    # unsafeMkPrivateKey

sampleKey :: PrivateKey
sampleKey =
  unsafeMkPrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"

isPrivateKey :: String -> Boolean
isPrivateKey s = isPrivateKeyImpl s