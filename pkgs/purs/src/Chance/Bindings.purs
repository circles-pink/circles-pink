module Chance.Bindings where

import Prelude
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Uncurried (EffectFn1)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Option (Option)

foreign import chance :: Chance

type Chance
  = { string ::
        EffectFn1
          ( Union2
              (Option ( length :: Int, casing :: Casing, alpha :: Boolean, numeric :: Boolean ))
              (Option ( length :: Int, pool :: String ))
          )
          String
    , name ::
        EffectFn1
          (Option ( middle :: Boolean, middle_initial :: Boolean, prefix :: Boolean, nationality :: Nationality ))
          String
    , integer ::
        EffectFn1
          (Option ( min :: Int, max :: Int ))
          Int
    }

type Casing
  = Union2 (String' "lower") (String' "upper")

type Nationality
  = Union2 (String' "en") (String' "it")

--------------------------------------------------------------------------------
-- Structural
--------------------------------------------------------------------------------
foreign import data Union2 :: Type -> Type -> Type

inj1of2 :: forall a b. a -> Union2 a b
inj1of2 = unsafeCoerce

inj2of2 :: forall a b. b -> Union2 a b
inj2of2 = unsafeCoerce

newtype String' :: forall k. k -> Type
newtype String' s
  = String' String

string' :: forall s. IsSymbol s => Proxy s -> String' s
string' s = String' $ reflectSymbol s
