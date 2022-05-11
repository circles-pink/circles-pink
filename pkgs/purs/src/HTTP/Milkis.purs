module HTTP.Milkis
  ( matchMethod
  , milkisRequest
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut (Json, stringify)
import Data.Bifunctor (lmap)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff, attempt)
import HTTP (ReqFn, _errNetwork, _errParseJson)
import Milkis (Response, URL(..), fetch, makeHeaders)
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

milkisRequest :: forall r. FetchImpl -> ReqFn r
milkisRequest fetchImpl req@{ url, method, body } = do
  resp <-
    fetch fetchImpl (URL url)
      { method: matchMethod method
      , body: stringify body
      , headers: makeHeaders { "Content-Type": "application/json" }
      }
      # attempt
      <#> lmap (const $ _errNetwork req)
      # ExceptT
  body' <-
    getJson resp
      # attempt
      <#> lmap (const _errParseJson)
      # ExceptT
  let
    status = M.statusCode resp
  pure { body: body', status }
  where
  getJson :: Response -> Aff Json
  getJson x = M.json x <#> unsafeCoerce
