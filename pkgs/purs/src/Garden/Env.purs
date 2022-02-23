module Garden.Env
  ( env
  ) where

import Prelude
import CirclesPink.StateMachine.Control as C
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import HTTP (ReqFn)
import Undefined (undefined)

env :: { request :: ReqFn } -> C.Env Aff
env { request } =
  { apiCheckUserName:
      \username ->
        request
          { url: "https://api.circles.gardenx/api/usersx"
          , method: POST
          , body: encodeJson { username }
          }
          <#> ( \result -> undefined
            -- case result of
            --       Left e -> undefined -- network error
            --       Right result' -> case (decodeJson result'.body) :: Either JsonDecodeError { username :: String } of
            --         Right x ->
            --           { username: username
            --           , isValid:
            --               case 200 of
            --                 404 -> undefined -- server error
            --                 200 -> true
            --                 _ -> false
            --           }
            --         Left _ -> undefined -- parse error
            )
  , apiCheckEmail:
      \email ->
        request
          { url: "https://api.circles.garden/api/users"
          , method: POST
          , body: encodeJson { email }
          }
          <#> (\_ -> true)
  }
