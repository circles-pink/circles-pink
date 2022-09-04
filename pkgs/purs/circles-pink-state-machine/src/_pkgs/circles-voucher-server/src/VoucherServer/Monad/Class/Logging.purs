module Logging where


-- class Log a where
  

-- --------------------------------------------------------------------------------

-- class Loggable a b where
--   logg :: a -> b


-- instance (Loggable b b', ToEncodeJson a a', EncodeJson a') => Loggable  (a -> b) (a -> b') where
--   logg f x = do
--     -- log "x"
--     logg $ f x

-- else instance MonadApp m => Loggable (m a) (m a) where
--   logg ma = do
--     r <- ma
--     -- log $ Log
--     pure r

-- else instance MonadApp m => Loggable a (m a) where
--   logg x = do
--     pure x


-- class EncodeJson b <= ToEncodeJson a b | a -> b where
--   toEncodeJson :: a -> b

-- instance ToEncodeJson Num Int where
--   toEncodeJson (Num x) = x 

-- newtype Num = Num Int

-- g :: Num -> Num
-- g x = x

-- f :: forall m. MonadApp m => Num -> m Num
-- f = logg g


-- -- logFn1 :: forall a1 a m. MonadApp m => (a1 -> m a) -> a1 -> m a
-- -- logFn1 f x = do
-- --   f x