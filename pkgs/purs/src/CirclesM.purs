module CirclesM (CirclesM, exec, act, input) where

import Prelude
import Control.Monad.State (StateT)
import Control.Monad.State as S
import Core.State.Onboard as O
import Effect.Aff (Aff)
import Undefined (undefined)

newtype CirclesM a
  = CirclesM (StateT O.State Aff a)

derive newtype instance functor :: Functor CirclesM

derive newtype instance apply :: Apply CirclesM

derive newtype instance applicative :: Applicative CirclesM

derive newtype instance bind :: Bind CirclesM

derive newtype instance monad :: Monad CirclesM

act :: O.Msg -> CirclesM Unit
act = undefined

input :: CirclesM String
input = undefined

exec :: forall a. CirclesM a -> O.State -> Aff O.State
exec (CirclesM m) s = S.execStateT m s
