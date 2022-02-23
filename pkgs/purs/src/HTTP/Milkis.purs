module HTTP.Milkis
  ( matchMethod
  , milkisRequest
  ) where

import Prelude
import Data.Argonaut (stringify)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (catchError)
import HTTP (NetworkError(..), ReqFn)
import Milkis (URL(..), fetch)
import Milkis as M
import Milkis.Impl (FetchImpl)
import Unsafe.Coerce (unsafeCoerce)

matchMethod :: Method -> M.Method
matchMethod = case _ of
  GET -> M.getMethod
  POST -> M.postMethod
  PUT -> M.putMethod
  DELETE -> M.deleteMethod
  _ -> M.getMethod

milkisRequest :: FetchImpl -> ReqFn
milkisRequest fetchImpl { url, method, body } =
  fetch fetchImpl (URL url)
    { method: matchMethod method
    , body: stringify body
    }
    <#> ( \res ->
          Right
            { body: unsafeCoerce $ M.json res
            , status: M.statusCode res
            }
      )
    # \m -> catchError m (\e -> pure $ Left ErrUnknown)
