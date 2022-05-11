module Web3.Bindings
  ( Eth
  , Utils
  , Web3
  , sendTransaction
  , web3
  ) where

import Prelude
import Effect.Aff.Compat (EffectFnAff(..))
import Effect.Uncurried (EffectFn1)

foreign import web3 :: Web3

type Web3
  = { utils :: Utils
    , eth :: Eth
    }

foreign import sendTransaction :: { from :: String, to :: String, value :: Number } -> EffectFnAff String

--------------------------------------------------------------------------------
type Utils
  = { toChecksumAddress :: String -> String
    }

type Eth
  = {
    }
