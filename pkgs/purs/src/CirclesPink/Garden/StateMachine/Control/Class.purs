module CirclesPink.Garden.StateMachine.Control.Class
  ( class CirclesControl
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Trans.Class (class MonadTrans)
import CirclesPink.Garden.StateMachine.Control.Env (Env)

class
  ( Monad m
  , MonadTrans t
  , Monad (t m)
  , MonadAsk (Env m) (t m)
  ) <= CirclesControl t m
