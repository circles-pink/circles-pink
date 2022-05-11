module Network.Ethereum.Core.Signatures.Extra
  ( ChecksumAddress
  , unChecksumAddress
  , unsafeFromString
  ) where

import Prelude
import Data.Maybe (fromJust)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import TypedEnv (class ParseValue)
import Undefined (undefined)

newtype ChecksumAddress
  = ChecksumAddress HexString

unsafeFromString :: Partial => String -> ChecksumAddress
unsafeFromString x = mkHexString x # fromJust # ChecksumAddress

unChecksumAddress :: ChecksumAddress -> HexString
unChecksumAddress (ChecksumAddress h) = h

instance parseValueAddress :: ParseValue ChecksumAddress where
  parseValue x = undefined
