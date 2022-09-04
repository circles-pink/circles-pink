module Tmp where

import Prelude

import Data.FpTs.Tuple (Tuple)
import Data.Symbol (reflectSymbol)
import Effect.Console (log)
import Type.Proxy (Proxy(..))


-- --type True :: forall a b. a -> b -> a
-- data True t f = True  t

-- --type False :: forall a b. a -> b -> b
-- data False t f = False f

-- --type Not :: forall a. (a -> a -> a) -> a -> a -> a
-- --type Not :: forall k1 k2 k3. (k1 -> k2 -> k3) -> k2 -> k1 -> k3
-- data Not b t f = Not (b f t) 

-- --type And :: forall a. (a -> b -> ) -> (a -> a -> a) -> (a -> a -> a)
-- --type And :: forall k1 k2 k3 k4. (k1 -> k2 -> k3 -> k2 -> k4) -> k1 -> k3 -> k2 -> k4
-- data And p q t f = And ( (p q f) t f)

-- --data Or :: forall k1 k2 k3 k4. (k1 -> k2 -> k1 -> k3 -> k4) -> k2 -> k1 -> k3 -> Type
-- data Or :: forall k1 k2 k3. (k1 -> k2 -> k1 -> k3 -> Type) -> k2 -> k1 -> k3 -> Type
-- data Or p q t f = Or ( (p t q) t f)



-- type Pair x y z = z x y 

-- type T = (And (True) (True)) "t" "f" -- (Or (Not True) True) "true" "false"

-- type Foo a b = Tuple a b

-- --type G = Foo

-- -- main = do
-- --   log $ reflectSymbol (Proxy :: _ T)