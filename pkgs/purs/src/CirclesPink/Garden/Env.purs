module CirclesPink.Garden.Env
  ( env
  ) where

import Prelude
import CirclesPink.Garden.StateMachine.Control as C
import CirclesPink.Garden.StateMachine.Error (CirclesError, CirclesError')
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Variant (inj)
import Effect.Aff (Aff)
import HTTP (ReqFn)
import Type.Proxy (Proxy(..))
import Wallet.PrivateKey as P

_errService :: CirclesError
_errService = inj (Proxy :: _ "errService") unit

_errParse :: CirclesError
_errParse = inj (Proxy :: _ "errParse") unit

env :: { request :: ReqFn (CirclesError' ()) } -> C.Env Aff
env { request } =
  { apiCheckUserName:
      \username ->
        if username == "" then
          pure { isValid: false }
        else
          request
            { url: "https://api.circles.garden/api/users/"
            , method: POST
            , body: encodeJson { username }
            }
            # runExceptT
            -- <#> (spy "log")
            
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
  , apiCheckEmail:
      \email ->
        if email == "" then
          pure { isValid: false }
        else
          request
            { url: "https://api.circles.garden/api/users/"
            , method: POST
            , body: encodeJson { email }
            }
            # runExceptT
            -- <#> (spy "log")
            
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
  , generatePrivateKey: P.genPrivateKey
  }
