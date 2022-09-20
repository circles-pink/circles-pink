module CirclesPink.Data.Nonce
  ( Nonce(..)
  , addressToNonce
  , nonceToBigInt
  , nonceToString
  ) where

import Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (Address)
import Data.BigInt (BigInt)
import Data.BigInt as B
import Data.Newtype (class Newtype, unwrap, wrap)

newtype Nonce = Nonce CC.Nonce

derive instance Newtype Nonce _

nonceToString :: Nonce -> String
nonceToString (Nonce n) = B.toString $ unwrap n

nonceToBigInt :: Nonce -> BigInt
nonceToBigInt (Nonce n) = unwrap n

addressToNonce :: Address -> Nonce
addressToNonce addr = Nonce $ wrap $ addressToNonceImpl $ show addr

foreign import addressToNonceImpl :: String -> BigInt