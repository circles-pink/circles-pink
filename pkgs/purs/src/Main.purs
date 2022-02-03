module Main
  ( main
  , mainAff
  ) where

import Prelude
import CirclesM (CirclesM)
import CirclesM as C
import Core.State.Onboard as O
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Garden.Env (env)

script :: CirclesM Unit
script = do
  C.act $ O.Next
  C.act $ O.SetUsername "hellohello"
  C.act $ O.Next
  C.act $ O.SetEmail "nico@hello.de"
  C.act $ O.SetPrivacy true
  C.act $ O.SetTerms true
  C.act $ O.Next

mainAff :: Aff O.State
mainAff = C.exec env script O.init

main :: Effect Unit
main = do
  runAff_ (const $ pure unit) mainAff
