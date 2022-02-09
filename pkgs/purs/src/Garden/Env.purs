module Garden.Env
  ( env
  ) where

import Prelude
import Affjax (printError)
import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
import Core.State.Onboard as O
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut.Core (fromObject, fromString, stringifyWithIndent)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Foreign.Object (fromFoldable)
import HTTP (ReqFn)
import Undefined (undefined)

env :: { request :: ReqFn } -> O.Env Aff
env { request } =
  { apiCheckUserName:
      \user -> do
        log $ "BBBchecking " <> user
        request
          { url: "https://api.circles.garden/api/users"
          , method: POST
          , body: encodeJson { username: user }
          }
          <#> ( \{ status, body } -> case status of
                0 -> true
                _ -> false
            )
  , apiCheckEmail:
      \email -> do
        log $ "checking " <> email
        result <-
          AX.request
            $ AX.defaultRequest
                { url = "https://api.circles.garden/api/users"
                , method = Left POST
                , responseFormat = ResponseFormat.json
                , content =
                  Just
                    $ Json
                    $ fromObject
                    $ fromFoldable [ "email" /\ fromString email ]
                }
        log ("API CALL EMAIL: " <> email)
        case result of
          Left err -> do
            log "ERROR"
            log $ printError err
            pure false
          Right { body } -> do
            log $ stringifyWithIndent 2 body
            pure true
  }
