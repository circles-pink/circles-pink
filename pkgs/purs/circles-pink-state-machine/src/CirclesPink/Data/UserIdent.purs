module CirclesPink.Data.UserIdent
  ( UserIdent(..)
  ) where


import CirclesCore (User)
import Data.Either (Either)
import Wallet.PrivateKey (Address)

type UserIdent = Either Address User