module Chance
  ( Casing(..)
  , FirstOpts
  , Nationality(..)
  , first
  , integer
  , name
  , stringPool
  ) where

import Prelude
import Chance.Bindings (chance, inj1of2, inj2of2, string')
import Chance.Bindings as B
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1)
import Option (class FromRecord, Option)
import Option as O
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
data Casing
  = Lower
  | Upper

getCasing :: Casing -> B.Casing
getCasing Lower = inj1of2 $ string' (Proxy :: _ "lower")

getCasing Upper = inj2of2 $ string' (Proxy :: _ "upper")

--------------------------------------------------------------------------------
data Nationality
  = En
  | It

getNationality :: Nationality -> B.Nationality
getNationality En = inj1of2 $ string' (Proxy :: _ "en")

getNationality It = inj2of2 $ string' (Proxy :: _ "it")

--------------------------------------------------------------------------------
type StringOpts = (length :: Int, casing :: Casing, alpha :: Boolean, numeric :: Boolean)

string :: forall r. FromRecord r () StringOpts => Record r -> Effect String
string x1 = runEffectFn1 chance.string $ inj1of2 x1'
  where
  x1' =
    x1
      # (O.fromRecord :: _ -> Option StringOpts)
      # O.modify (Proxy :: _ "casing") getCasing

--------------------------------------------------------------------------------
type StringPoolOpts = (length :: Int, pool :: String)

stringPool :: forall r. FromRecord r () StringPoolOpts => Record r -> Effect String
stringPool x1 = runEffectFn1 chance.string $ inj2of2 x1'
  where
  x1' =
    x1
      # (O.fromRecord :: _ -> Option StringPoolOpts)

--------------------------------------------------------------------------------
type IntegerOpts = (min :: Int, max :: Int)

integer :: forall r. FromRecord r () IntegerOpts => Record r -> Aff Int
integer x1 = runEffectFn1 chance.integer x1' # liftEffect
  where
  x1' =
    x1
      # (O.fromRecord :: _ -> Option IntegerOpts)

--------------------------------------------------------------------------------
type NameOpts = (middle :: Boolean, middle_initial :: Boolean, prefix :: Boolean, nationality :: Nationality)

name :: forall r. FromRecord r () NameOpts => Record r -> Aff String
name x1 = runEffectFn1 chance.name x1' # liftEffect
  where
  x1' =
    x1
      # (O.fromRecord :: _ -> Option NameOpts)
      # O.modify (Proxy :: _ "nationality") getNationality

--------------------------------------------------------------------------------
type FirstOpts = (nationality :: Nationality)

first :: forall r. FromRecord r () FirstOpts => Record r -> Aff String
first x1 = runEffectFn1 chance.first x1' # liftEffect
  where
  x1' =
    x1
      # (O.fromRecord :: _ -> Option FirstOpts)
      # O.modify (Proxy :: _ "nationality") getNationality

--------------------------------------------------------------------------------
