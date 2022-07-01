module EncryptStorage.Bindings where

import Prelude

import Effect (Effect)

newtype EsOptions = EsOptions
  { prefix :: String
  }

newtype SecretKey = SecretKey String

foreign import data ES :: Type

foreign import newEs :: SecretKey -> EsOptions -> Effect ES

foreign import setItem :: ES -> String -> String -> Effect Unit

foreign import getItem :: forall z. z -> (String -> z) -> ES -> String -> Effect z

foreign import removeItem :: ES -> String -> Effect Unit