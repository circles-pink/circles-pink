module CirclesPink.Garden.StateMachine.Control.Class.ProdM
  ( ProdM
  , runProdM
  )
  where

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

derive newtype instance functor :: Functor ProdM

derive newtype instance apply :: Apply ProdM

derive newtype instance applicative :: Applicative ProdM

derive newtype instance monad :: Monad ProdM

derive newtype instance bind :: Bind ProdM

derive newtype instance monadEffect :: MonadEffect ProdM

derive newtype instance monadAff :: MonadAff ProdM

derive newtype instance monadAsk :: MonadAsk (CirclesConfig ProdM) ProdM


instance monadCircles :: MonadCircles ProdM where
  sleep i = liftAff $ delay $ Milliseconds $ I.toNumber i 

instance monadLog :: MonadLog ProdM where
  log x = liftAff $ A.log x  

runProdM :: forall a. CirclesConfig ProdM -> ProdM a -> Aff a
runProdM cfg (ProdM x) = runReaderT x cfg