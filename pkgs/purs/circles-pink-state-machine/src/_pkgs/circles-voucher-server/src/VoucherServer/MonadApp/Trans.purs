module VoucherServer.MonadAppApp.Trans where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Debug.Extra (todo)
import Payload.ResponseTypes (Response)
import VoucherServer.MonadApp.Class (AppEnv, AppError)


type Error = Response AppError

type Env m = AppEnv (AppT m)

newtype AppT m a = AppT 
  ( ReaderT (Env m)
      (ExceptT Error m)
      a
  )

runAppT :: forall m a. AppT m a -> Env m -> m (Either Error a)
runAppT = todo