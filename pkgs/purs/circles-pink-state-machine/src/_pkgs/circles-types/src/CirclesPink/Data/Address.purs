module CirclesPink.Data.Address
  ( Address(..)
  , module Exp
  , parseAddress
  , sampleAddress
  , sampleSafeAddress
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, wrap)
import Data.String.Utils as Str
import FpTs.Class (class FpTs)
import GraphQL.Client.Args (class ArgGql)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (mkAddress) as Exp
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen as G
import TypedEnv (class ParseValue)

--import Payload.Server.Params (class DecodeParam, decodeParam)
--import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Address = Address W3.Address

derive instance newtype_ :: Newtype Address _
derive newtype instance show :: Show Address
derive newtype instance eq :: Eq Address
derive newtype instance ord :: Ord Address
derive newtype instance decodeJson :: DecodeJson Address
derive newtype instance encodeJson :: EncodeJson Address
derive newtype instance readForeignAddress :: ReadForeign Address
derive newtype instance writeForeignAddress :: WriteForeign Address

instance arbitraryAddr :: Arbitrary Address where
  arbitrary = unsafePartial do
    let
      chars = "0123456789abcdef"
      len = 40
      chars' = chars # Str.toCharArray # NEA.fromArray # fromJust
    str <- chars' <#> pure # G.oneOf # G.vectorOf len <#> Str.fromCharArray
    parseAddress str # fromJust # pure

instance parseValueAddress :: ParseValue Address where
  parseValue = parseAddress

instance fpTs :: FpTs Address Address where
  toFpTs = identity
  fromFpTs = identity

-- instance decodeParamAddress :: DecodeParam Address where
--   decodeParam x = decodeParam x
--     >>= (parseAddress >>> note "Could not parse Address")

instance argGqlAddress :: ArgGql Address String

sampleAddress :: Address
sampleAddress = unsafeMkAddress "fb7dc4d8f841af32d777e698d6c71409e85955d9"

sampleSafeAddress :: Address
sampleSafeAddress = unsafeMkAddress "984501180D63335928eA7fb59c17d33e0398Ed39"

parseAddress :: String -> Maybe Address
parseAddress x = x
  # mkHexString
  >>= W3.mkAddress
  <#> wrap

unsafeMkAddress :: String -> Address
unsafeMkAddress s = unsafePartial
  $ wrap
  $ fromJust
  $ W3.mkAddress
  $ unsafeMkHexString s

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s
