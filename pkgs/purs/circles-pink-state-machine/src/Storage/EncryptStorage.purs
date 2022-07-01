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

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, jsonParser, stringify)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Maybe (Maybe(..), fromJust)
import Data.Variant (Variant, inj)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Class (liftEffect)
import Storage.EncryptStorage.Bindings (EsOptions, SecretKey, ES) as Exp
import Storage.EncryptStorage.Bindings as B
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newEs :: B.SecretKey -> B.EsOptions -> Effect B.ES
newEs = B.newEs

setItem :: forall k v. EncodeJson k => EncodeJson v => B.ES -> k -> v -> Effect Unit
setItem es k v = B.setItem es (stringify $ encodeJson k) (stringify $ encodeJson v)

type ErrGetItem r = ErrItemNotFound + ErrParseJson + ErrParse + r

getItem :: forall k v r. EncodeJson k => EncodeJson v => B.ES -> k -> ExceptT (Variant (ErrGetItem r)) Effect Json
getItem es k =
  let
    key = stringify $ encodeJson k
  in
    B.getItem Nothing Just es key
      <#> note (_errItemNotFound key)
      <#> (\et -> et >>= jsonParser # lmap _errParseJson)
      # ExceptT

--   # decodeJson
--   # stringify

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