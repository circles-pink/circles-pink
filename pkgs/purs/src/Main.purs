module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello"

myApi :: Int -> String -> Boolean
myApi x y = (x > 5) && (y /= "Hello")
