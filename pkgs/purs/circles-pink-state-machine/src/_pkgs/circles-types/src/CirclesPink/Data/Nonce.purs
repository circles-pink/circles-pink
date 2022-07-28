module CirclesPink.Data.Nonce
  ( Nonce
  , addressToNonce
  , nonceToBigInt
  , nonceToString
  ) where

import CirclesPink.Data.Address (Address)
import Data.BigInt (BigInt)
import Data.BigInt as B

import Prelude

newtype Nonce = Nonce BigInt

nonceToString :: Nonce -> String
nonceToString (Nonce n) = B.toString n

nonceToBigInt :: Nonce -> BigInt
nonceToBigInt (Nonce n) = n

addressToNonce :: Address -> Nonce
addressToNonce addr = Nonce $ addressToNonceImpl $ show addr

foreign import addressToNonceImpl :: String -> BigInt