module Web3 where

import Prelude
import Web3.Bindings as B
import Effect.Aff (Aff)
import Effect.Aff.Compat (fromEffectFnAff)

sendTransaction :: { from :: String, to :: String, value :: Number } -> Aff String
sendTransaction opts = fromEffectFnAff $ B.sendTransaction opts
