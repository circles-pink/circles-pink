module Web3.Bindings where

import Prelude

foreign import web3 :: Web3

type Web3
  = { utils :: Utils
    }

--------------------------------------------------------------------------------
type Utils
  = { toChecksumAddress :: String -> String
    }
