module Garden.Env
  ( env
  ) where

import Prelude
import CirclesPink.StateMachine.Control as C
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import HTTP (ReqFn)

env :: { request :: ReqFn } -> C.Env Aff
env { request } =
  { apiCheckUserName:
      \username ->
        request
          { url: "https://api.circles.garden/api/usersx"
          , method: POST
          , body: encodeJson { username }
          }
          <#> ( \{ status, body } ->
                let
                  result :: Either JsonDecodeError { username :: String }
                  result = decodeJson body
                in
                  { username: username
                  , isValid:
                      case status of
                        200 -> true
                        _ -> false
                  }
            )
  , apiCheckEmail:
      \email ->
        request
          { url: "https://api.circles.garden/api/users"
          , method: POST
          , body: encodeJson { email }
          }
          <#> ( \{ status } -> case status of
                200 -> true
                _ -> false
            )
  }
