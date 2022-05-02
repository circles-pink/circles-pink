module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

-- script :: CirclesM Unit
-- script = do
--   C.act $ O.Next
--   C.act $ O.SetUsername "hellohello"
--   C.act $ O.Next
--   C.act $ O.SetEmail "nico@hello.de"
--   C.act $ O.SetPrivacy true
--   C.act $ O.SetTerms true
--   C.act $ O.Next
-- mainAff :: Aff O.State
-- mainAff = C.exec (env { request: HM.milkisRequest nodeFetch }) script O.init
main :: Effect Unit
main = do
  log ".."

--runAff_ (const $ pure unit) mainAff
