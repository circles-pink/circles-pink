module Main where

import Prelude
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Undefined (undefined)

--------------------------------------------------------------------------------
class TS a where
  toTs :: a -> String

instance numberTS :: TS Number where
  toTs _ = "number"

instance intTS :: TS Int where
  toTs _ = "number"

instance stringTS :: TS String where
  toTs _ = "string"

instance charTS :: TS Char where
  toTs _ = "string"

instance booleanTS :: TS Boolean where
  toTs _ = "boolean"

instance arrayTS :: TS b => TS (Array b) where
  toTs _ = "Array<" <> toTs (undefined :: b) <> ">"

instance tupleTS :: (TS a, TS b) => TS (Tuple a b) where
  toTs _ = "[" <> toTs (undefined :: a) <> ", " <> toTs (undefined :: b) <> "]"

buildFn :: Array String -> String -> String
buildFn args t =
  fold
    (mapWithIndex buildStr args)
    <> t
  where
  buildStr i x =
    "(x"
      <> show (i + 1)
      <> ": "
      <> x
      <> ") => "

instance fn3TS :: (TS a1, TS a2, TS a3, TS b) => TS (Function a1 (Function a2 (Function a3 b))) where
  toTs _ =
    buildFn
      [ toTs (undefined :: a1)
      , toTs (undefined :: a2)
      , toTs (undefined :: a3)
      ]
      (toTs (undefined :: b))
else instance fn2TS :: (TS a1, TS a2, TS b) => TS (Function a1 (Function a2 b)) where
  toTs _ = buildFn [ toTs (undefined :: a1), toTs (undefined :: a2) ] (toTs (undefined :: b))
else instance fn1TS :: (TS a1, TS b) => TS (Function a1 b) where
  toTs _ = buildFn [ toTs (undefined :: a1) ] (toTs (undefined :: b))

tsDeclare :: forall a. TS a => String -> a -> String
tsDeclare nm tscode = "export declare const " <> nm <> ": " <> toTs tscode

--------------------------------------------------------------------------------
myApi :: Int -> String -> Boolean
myApi x y = (x > 5) && (y /= "Hello")

myApi_ :: Function Int (Function String Boolean)
myApi_ x y = (x > 5) && (y /= "Hello")

f1 :: Int -> String
f1 _ = ""

gravity :: Number
gravity = 9.81

name :: String
name = "Foo"

hobbies :: Array String
hobbies = [ "lesen", "fussball" ]

ages :: Array Number
ages = [ 3.0, 5.0, 6.0 ]

circles :: Tuple Number String
circles = Tuple 13.0 "Hello"

--------------------------------------------------------------------------------
main :: Effect Unit
main = do
  log $ tsDeclare "gravity" gravity
  log $ tsDeclare "name" name
  log $ tsDeclare "hobbies" hobbies
  log $ tsDeclare "ages" ages
  log $ tsDeclare "circles" circles
  log $ tsDeclare "myApi" myApi
