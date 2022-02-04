module HTTP.Milkis where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, encodeJson, stringify)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import HTTP (ReqFn)
import Milkis (URL(..), fetch)
import Milkis as M
import Milkis.Impl (FetchImpl)
import Milkis.Impl.Window (windowFetch)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

matchMethod :: Method -> M.Method
matchMethod = case _ of
  GET -> M.getMethod
  POST -> M.postMethod
  PUT -> M.putMethod
  DELETE -> M.deleteMethod
  _ -> M.getMethod

request :: FetchImpl -> ReqFn
request fetchImpl { url, method, body } parser =
  ( \res ->
      parser
        { body: unsafeCoerce $ M.json res
        , status: M.statusCode res
        }
  )
    <$> fetch fetchImpl (URL url)
        { method: matchMethod method
        , body: stringify body
        }
