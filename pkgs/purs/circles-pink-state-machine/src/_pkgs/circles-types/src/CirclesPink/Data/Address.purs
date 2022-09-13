module CirclesPink.Data.Address
  ( Address(..)
  , addrToString
  , mkAddress
  , module Exp
  , parseAddress
  , sampleAddress
  , sampleSafeAddress
  )
  where

import CirclesPink.Prelude

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

derive instance Newtype Address _
derive newtype instance Show Address
derive newtype instance Eq Address
derive newtype instance Ord Address
derive newtype instance DecodeJson Address
derive newtype instance EncodeJson Address
derive newtype instance ReadForeign Address
derive newtype instance WriteForeign Address

instance Arbitrary Address where
  arbitrary = unsafePartial do
    let
      chars = "0123456789abcdef"
      len = 40
      chars' = chars # Str.toCharArray # NEA.fromArray # fromJust
    str <- chars' <#> pure # G.oneOf # G.vectorOf len <#> Str.fromCharArray
    parseAddress str # fromJust # pure

instance ParseValue Address where
  parseValue = parseAddress

ptAddress :: PursType
ptAddress = PursType "CirclesPink_Data_Address" "Address"

instance ToTsDef Address where
  toTsDef _ = defaultToTsDef ptAddress []

instance ToTsType Address where
  toTsType _ = defaultToTsType ptAddress []

instance ToPursType Address where
  toPursType _ = defaultToPursType ptAddress []

instance FpTs Address Address where
  toFpTs = identity
  fromFpTs = identity

-- instance decodeParamAddress :: DecodeParam Address where
--   decodeParam x = decodeParam x
--     >>= (parseAddress >>> note "Could not parse Address")

instance ArgGql Address String

sampleAddress :: Address
sampleAddress = unsafeMkAddress "fb7dc4d8f841af32d777e698d6c71409e85955d9"

sampleSafeAddress :: Address
sampleSafeAddress = unsafeMkAddress "984501180D63335928eA7fb59c17d33e0398Ed39"

parseAddress :: String -> Maybe Address
parseAddress x = x
  # mkHexString
  >>= W3.mkAddress
  <#> wrap

addrToString :: Address -> String
addrToString = show

unsafeMkAddress :: String -> Address
unsafeMkAddress s = unsafePartial
  $ wrap
  $ fromJust
  $ W3.mkAddress
  $ unsafeMkHexString s

unsafeMkHexString :: String -> HexString
unsafeMkHexString s = unsafePartial $ fromJust $ mkHexString s
