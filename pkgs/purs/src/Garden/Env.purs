module Garden.Env
  ( env
  ) where

import Prelude
import Data.Argonaut (encodeJson)
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import HTTP (ReqFn)
import CirclesPink.StateMachine.Control as C

env :: { request :: ReqFn } -> C.Env Aff
env { request } =
  { apiCheckUserName:
      \username ->
        request
          { url: "https://api.circles.garden/api/users"
          , method: POST
          , body: encodeJson { username }
          }
          <#> ( \{ status } -> case status of
                200 -> true
                _ -> false
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
