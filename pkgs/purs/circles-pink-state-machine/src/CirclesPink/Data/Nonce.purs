module CirclesPink.Data.Nonce
  ( Nonce
  , nonceToBigInt
  , nonceToString
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as B

newtype Nonce = Nonce BigInt

nonceToString :: Nonce -> String
nonceToString (Nonce n) = B.toString n

nonceToBigInt :: Nonce -> BigInt
nonceToBigInt (Nonce n) = n
