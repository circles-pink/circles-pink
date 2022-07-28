module CirclesPink.Data.PrivateKey
  ( genPrivateKey
  , mnemonicToKey
  , module Exp
  , sampleKey
  )
  where

import Prelude

import CirclesPink.Data.Mnemonic (Mnemonic)
import CirclesPink.Data.Mnemonic as M
import CirclesPink.Data.PrivateKey.Type (PrivateKey)
import CirclesPink.Data.PrivateKey.Type (PrivateKey(..)) as Exp
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkPrivateKey)
import Partial.Unsafe (unsafePartial)

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
mnemonicToKey mn = unsafePartial result
  where
  result :: Partial => PrivateKey
  result = mn
    # M.getWords
    # S.joinWith " "
    # mnemonicToEntropyImpl Nothing Just
    # fromJust
    # unsafeMkPrivateKey

sampleKey :: PrivateKey
sampleKey =
  unsafeMkPrivateKey "68135baae5b1856359041566a8d32c0374b355a4f12dd7a0690d00b76559e19c"

foreign import mnemonicToEntropyImpl :: Maybe String -> (String -> Maybe String) -> String -> Maybe String
