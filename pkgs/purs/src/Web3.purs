module Web3
  ( sendTransaction
  ) where

import Prelude
import Data.Maybe (fromJust)
import Effect.Aff (Aff)
import Effect.Aff.Compat (fromEffectFnAff)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import Web3.Bindings as B

sendTransaction :: { from :: W3.Address, to :: W3.Address, value :: Number } -> Aff HexString
sendTransaction opts =
  unsafePartial
    ( B.sendTransaction { from: show opts.from, to: show opts.to, value: opts.value }
        # fromEffectFnAff
        <#> (mkHexString >>> fromJust)
    )
