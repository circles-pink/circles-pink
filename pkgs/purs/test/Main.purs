module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Wallet.PrivateKey as Wallet.PrivateKey

main :: Effect Unit
main =
  runTest do
    Wallet.PrivateKey.tests
