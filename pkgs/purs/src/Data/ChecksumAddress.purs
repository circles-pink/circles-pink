module Data.ChecksumAddress
  ( ChecksumAddress
  ) where

import Prelude
import Network.Ethereum.Core.HexString (HexString)

newtype ChecksumAddress
  = ChecksumAddress HexString
