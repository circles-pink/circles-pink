module CirclesPink.Garden.StateMachine.Control.Class.ProdM
  ( ProdM
  , runProdM
  ) where

import Prelude

import CirclesPink.Garden.StateMachine (CirclesConfig)
import CirclesPink.Garden.StateMachine.Control.Class (class MonadCircles)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Int as I
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as A
import Log.Class (class MonadLog)

newtype ProdM a = ProdM (ReaderT (CirclesConfig ProdM) Aff a)

derive newtype instance Functor ProdM

derive newtype instance Apply ProdM

derive newtype instance Applicative ProdM

derive newtype instance Monad ProdM

derive newtype instance Bind ProdM

derive newtype instance MonadEffect ProdM

derive newtype instance MonadAff ProdM

derive newtype instance MonadAsk (CirclesConfig ProdM) ProdM

instance MonadCircles ProdM where
  sleep i = liftAff $ delay $ Milliseconds $ I.toNumber i

instance MonadLog ProdM where
  log x = liftAff $ A.log x

runProdM :: forall a. CirclesConfig ProdM -> ProdM a -> Aff a
runProdM cfg (ProdM x) = runReaderT x cfg