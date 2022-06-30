module Data.FpTs.Either
  ( Either(..)
  , Left(..)
  , Right(..)
  , left
  , match
  , right
  ) where

import Prelude

import Data.Either as P
import Data.FpTs.Union (type (-|-))
import Data.FpTs.Union as U
import FpTs.Class (class FpTs, fromFpTs, toFpTs)
import Type.Data.String (String', mkString')
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

newtype Left a = Left { _tag :: String' "Left", left :: a }

newtype Right b = Right { _tag :: String' "Right", right :: b }

newtype Either a b = Either (Left a -|- Right b)

instance fpTsEither :: (FpTs a a', FpTs b b') => FpTs (P.Either a b) (Either a' b') where
  toFpTs = P.either (toFpTs >>> left) (toFpTs >>> right)
  fromFpTs = match (fromFpTs >>> P.Left) (fromFpTs >>> P.Right)

--------------------------------------------------------------------------------

left :: forall a b. a -> Either a b
left x = Either $ U.left $ Left { _tag: mkString' (Proxy :: _ "Left"), left: x }

right :: forall a b. b -> Either a b
right x = Either $ U.right $ Right { _tag: mkString' (Proxy :: _ "Right"), right: x }

match :: forall a b z. (a -> z) -> (b -> z) -> Either a b -> z
match onLeft onRight (Either x) = case (unsafeCoerce x)._tag of
  "Left" -> onLeft (unsafeCoerce x).left
  "Right" -> onRight (unsafeCoerce x).right
  _ -> undefined