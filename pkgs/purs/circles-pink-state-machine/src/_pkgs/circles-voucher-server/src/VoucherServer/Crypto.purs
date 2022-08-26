module VoucherServer.Crypto
  ( encrypt
  , decrypt
  ) where

import Data.Maybe (Maybe(..))

foreign import decryptImpl :: forall z. z -> (String -> z) -> String -> String -> z

decrypt :: String -> String -> Maybe String
decrypt = decryptImpl Nothing Just

foreign import encryptImpl :: forall z. z -> (String -> z) -> String -> String -> z

encrypt :: String -> String -> Maybe String
encrypt = encryptImpl Nothing Just
