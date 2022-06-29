module CirclesPink.Data.Trust
  ( Trust
  ) where

import CirclesPink.Data.TrustState (TrustState)
import CirclesPink.Data.UserIdent (UserIdent)

type Trust =
  { isOutgoing :: Boolean
  , user :: UserIdent
  , trustState :: TrustState
  }