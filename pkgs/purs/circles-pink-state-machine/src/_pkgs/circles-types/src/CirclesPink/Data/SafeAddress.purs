module CirclesPink.Data.SafeAddress where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.Address as C
import CirclesPink.Data.Address as W3
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import GraphQL.Client.Args (class ArgGql)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (Address)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class WriteForeign)

--------------------------------------------------------------------------------

newtype SafeAddress = SafeAddress CC.SafeAddress

derive instance Newtype SafeAddress _

derive newtype instance Ord SafeAddress
derive newtype instance Eq SafeAddress
derive newtype instance Show SafeAddress
derive newtype instance WriteForeign SafeAddress
derive newtype instance DecodeJson SafeAddress
derive newtype instance EncodeJson SafeAddress

-- instance DecodeParam SafeAddress where
--   decodeParam x = decodeParam x
--     >>= (parseAddress >>> note "Could not parse SafeAddress")
--     <#> SafeAddress

-- instance EncodeParam SafeAddress where
--   encodeParam (SafeAddress x) = todo -- encodeParam x

instance ArgGql SafeAddress String

sampleSafeAddress :: SafeAddress
sampleSafeAddress = SafeAddress $ CC.SafeAddress $ unsafeMkAddress "fd0819dc0ad02411258939c00303ead2789795ac"

unsafeMkAddress :: String -> Address
unsafeMkAddress s = unsafePartial
  $ fromJust
  $ W3.mkAddress
  $ unsafeMkHexString s

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s