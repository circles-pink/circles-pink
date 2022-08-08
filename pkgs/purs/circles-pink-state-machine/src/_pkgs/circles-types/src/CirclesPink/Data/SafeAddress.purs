module CirclesPink.Data.SafeAddress where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.Address as C
import CirclesPink.Data.Address as W3
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import GraphQL.Client.Args (class ArgGql)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (Address)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class WriteForeign)

--------------------------------------------------------------------------------

newtype SafeAddress = SafeAddress CC.SafeAddress

derive instance newtypeSafeAddress :: Newtype SafeAddress _

derive newtype instance ordSafeAddress :: Ord SafeAddress
derive newtype instance eqSafeAddress :: Eq SafeAddress
derive newtype instance showSafeAddress :: Show SafeAddress
derive newtype instance writeForeignSafeAddress :: WriteForeign SafeAddress

-- instance decodeParamSafeAddress :: DecodeParam SafeAddress where
--   decodeParam x = decodeParam x
--     >>= (parseAddress >>> note "Could not parse SafeAddress")
--     <#> SafeAddress

-- instance encodeParamSafeAddress :: EncodeParam SafeAddress where
--   encodeParam (SafeAddress x) = todo -- encodeParam x

instance argGqlSafeAddress :: ArgGql SafeAddress String

sampleSafeAddress :: SafeAddress
sampleSafeAddress = SafeAddress $ CC.SafeAddress $ unsafeMkAddress "fd0819dc0ad02411258939c00303ead2789795ac"

unsafeMkAddress :: String -> Address
unsafeMkAddress s = unsafePartial
  $ fromJust
  $ W3.mkAddress
  $ unsafeMkHexString s

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s