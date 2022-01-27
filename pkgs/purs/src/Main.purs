module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Undefined (undefined)

--------------------------------------------------------------------------------
class TS a where
  toTs :: a -> String

instance numberTS :: TS Number where
  toTs _ = "number"

instance stringTS :: TS String where
  toTs _ = "string"

instance arrayTS :: TS b => TS (Array b) where
  toTs _ = "Array<" <> toTs (undefined :: b) <> ">"

tsDeclare :: forall a. TS a => String -> a -> String
tsDeclare nm tscode = "export declare const " <> nm <> ": " <> toTs tscode

--------------------------------------------------------------------------------
myApi :: Int -> String -> Boolean
myApi x y = (x > 5) && (y /= "Hello")

gravity :: Number
gravity = 9.81

name :: String
name = "Foo"

hobbies :: Array String
hobbies = [ "lesen", "fussball" ]

ages :: Array Number
ages = [ 3.0, 5.0, 6.0 ]

api =
  { myApi
  , gravity
  }

--------------------------------------------------------------------------------
main :: Effect Unit
main = do
  log $ tsDeclare "gravity" gravity
  log $ tsDeclare "name" name
  log $ tsDeclare "hobbies" hobbies
  log $ tsDeclare "ages" ages
