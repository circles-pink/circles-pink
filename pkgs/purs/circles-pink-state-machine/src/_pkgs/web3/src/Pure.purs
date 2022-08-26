module Web3.Pure where

import Prelude

import CirclesPink.Data.Address (Address, parseAddress)
import Control.Monad.Error.Class (try)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect.Unsafe (unsafePerformEffect)
import Web3 (Hash(..), Message(..), SignatureObj, Web3)
import Web3 as W
import Web3.Bindings as B

web3 :: Web3
web3 = unsafePerformEffect B.newWeb3_

accountsRecover ::  SignatureObj -> Maybe Address
accountsRecover so = so
  # W.fromSignatureObj
  # B.accountsRecover web3
  # try
  # unsafePerformEffect
  # hush
  >>= parseAddress

accountsHashMessage :: Message -> Hash
accountsHashMessage (Message msg) = B.accountsHashMessage web3 msg # Hash