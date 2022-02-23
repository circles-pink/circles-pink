module Garden.Env
  ( env
  ) where

import Prelude
import CirclesPink.StateMachine.Control as C
import CirlesPink.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (ExceptT(..), except, mapExcept, mapExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import HTTP (NetworkError, Req, ReqFn, Res, _errUnknown)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Undefined (undefined)

_errService :: forall r. Variant (CirclesError + r)
_errService = inj (Proxy :: _ "errService") unit

_errParse :: forall r. Variant (CirclesError + r)
_errParse = inj (Proxy :: _ "errParse") unit

env ∷
  { request ∷ ReqFn (CirclesError' ()) } -> C.Env Aff
env { request } =
  { apiCheckUserName:
      \username ->
        request
          { url: "https://api.circles.garden/api/users"
          , method: POST
          , body: encodeJson { username }
          }
          -- <#> ( \x -> do
          
          --       logShow x
          
          --       pure x
          
          --   )
          
          # runExceptT
          <#> ( \result -> case result of
                Left e -> Left e
                Right x
                  | x.status /= 200 && x.status /= 409 -> Left $ _errService
                  | otherwise -> Right x
            )
          <#> ( \result -> do
                res <- result
                body' :: { status :: String } <- decodeJson res.body # lmap (const _errParse)
                if body'.status == "ok" then
                  Right { isValid: true }
                else
                  Right { isValid: false }
            )
          # ExceptT
  , apiCheckEmail: undefined
  -- \email ->
  --   request 
  --     { url: "https://api.circles.garden/api/users"
  --     , method: POST
  --     , body: encodeJson { email }
  --     }
  --     <#> (\_ -> true)
  }
