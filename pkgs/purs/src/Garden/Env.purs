module Garden.Env
  ( env
  ) where

import Prelude
import CirclesPink.StateMachine.Control as C
import CirlesPink.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Variant (Variant, inj)
import Debug (spy)
import Effect.Aff (Aff)
import HTTP (ReqFn)
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
          { url: "https://api.circles.garden/api/users/"
          , method: POST
          , body: encodeJson { username }
          }
          # runExceptT
          <#> (spy "log")
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
