-- | The Sea module provides basic security and authentication functionality.
module GunDB.Sea
  ( auth
  , create
  ) where

import Prelude (Unit)
import GunDB (GunDb, User)
import Effect.Aff (Aff)
import Data.Function.Uncurried (Fn3, runFn3)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

-- | Authenticates a user with a username and a password.
auth :: String -> String -> GunDb -> Aff User
auth usr pwd gundb = fromEffectFnAff (runFn3 _auth gundb usr pwd)

foreign import _auth :: Fn3 GunDb String String (EffectFnAff User)

-- | Creates a new user with the given username and password.
create :: String -> String -> GunDb -> Aff Unit
create usr pwd gundb = fromEffectFnAff (runFn3 _create gundb usr pwd)

foreign import _create :: Fn3 GunDb String String (EffectFnAff Unit)
