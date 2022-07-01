module Storage.EncryptStorage
  ( ErrGetItem
  , ErrItemNotFound
  , ErrParse
  , ErrParseJson
  , getItem
  , module Exp
  , newEs
  , setItem
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), except, mapExceptT)
import Control.Monad.Except.Checked (safe)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError, decodeJson, encodeJson, jsonParser, stringify)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Storage.EncryptStorage.Bindings (EsOptions, SecretKey, ES) as Exp
import Storage.EncryptStorage.Bindings as B
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newEs :: forall r. B.SecretKey -> B.EsOptions -> ExceptT (Variant r) Aff B.ES
newEs sk o = B.newEs sk o # liftEffect

setItem :: forall r k v. EncodeJson k => EncodeJson v => B.ES -> k -> v -> ExceptT (Variant r) Aff Unit
setItem es k v = B.setItem es (stringify $ encodeJson k) (stringify $ encodeJson v)
  # liftEffect

--------------------------------------------------------------------------------
type ErrGetItem r = ErrItemNotFound + ErrParseJson + ErrParse + r

getItem :: forall k v r. EncodeJson k => DecodeJson v => B.ES -> k -> ExceptT (Variant (ErrGetItem r)) Aff v
getItem es k =
  let
    key = stringify $ encodeJson k
  in
    (B.getItem Nothing Just es key <#> note (_errItemNotFound key) # ExceptT)
      >>= (jsonParser >>> lmap _errParseJson >>> except)
      >>= (decodeJson >>> lmap _errParse >>> except)
      # mapExceptT liftEffect

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

type ErrItemNotFound r = (errItemNotFound :: String | r)

type ErrParseJson r = (errParseJson :: String | r)

type ErrParse r = (errParse :: JsonDecodeError | r)

--------------------------------------------------------------------------------
-- Err constructors
--------------------------------------------------------------------------------

_errItemNotFound :: forall r. String -> Variant (ErrItemNotFound r)
_errItemNotFound = inj (Proxy :: _ "errItemNotFound")

_errParseJson :: forall r. String -> Variant (ErrParseJson r)
_errParseJson = inj (Proxy :: _ "errParseJson")

_errParse :: forall r. JsonDecodeError -> Variant (ErrParse r)
_errParse = inj (Proxy :: _ "errParse")