module CirclesPink.Data.SafeAddress where

import Prelude

import CirclesCore as CC
import Data.Newtype (class Newtype)
import Debug.Extra (todo)
import GraphQL.Client.Args (class ArgGql)
import Payload.Client.EncodeParam (class EncodeParam)
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

instance encodeParamSafeAddress :: EncodeParam SafeAddress where
  encodeParam (SafeAddress x) = todo -- encodeParam x

instance argGqlSafeAddress :: ArgGql SafeAddress String


sampleSafeAddress :: SafeAddress
sampleSafeAddress = todo
