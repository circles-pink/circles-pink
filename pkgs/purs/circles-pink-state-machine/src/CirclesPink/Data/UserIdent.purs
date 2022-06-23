module CirclesPink.Data.UserIdent
  ( UserIdent(..)
  , getAddress
  ) where

import CirclesCore (User)
import Data.Either (Either(..))
import Wallet.PrivateKey (Address)

type UserIdent = Either Address User

getAddress :: UserIdent -> Address
getAddress (Left addr) = addr
getAddress (Right { safeAddress }) = safeAddress
