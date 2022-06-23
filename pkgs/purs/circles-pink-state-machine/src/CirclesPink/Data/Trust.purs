module CirclesPink.Data.Trust
  ( Trust
  )
  where


import CirclesCore (User)
import Data.Either (Either)
import Network.Ethereum.Core.Signatures as W3
import CirclesPink.Data.TrustState (TrustState)

type Trust =
  { isOutgoing :: Boolean
  , user :: Either W3.Address User
  , trustState :: TrustState
  }