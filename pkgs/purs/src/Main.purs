module Main
  ( env
  , main
  , mainAff
  ) where

import Prelude
import Affjax (printError)
import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
import CirclesM (CirclesM)
import CirclesM as C
import Core.State.Onboard as O
import Data.Argonaut.Core (fromObject, fromString, stringifyWithIndent)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Foreign.Object (fromFoldable)
import Undefined (undefined)

env :: O.Env Aff
env =
  { apiCheckUserName:
      \user -> do
        log $ "BBBchecking " <> user
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
                    $ fromFoldable [ "username" /\ fromString user ]
                }
        log ("API CALL USERNAME: " <> user)
        case result of
          Left e -> do
            log "ERROR"
            log $ printError e
            pure false
          Right { body } -> do
            log $ stringifyWithIndent 2 body
            pure true
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

script :: CirclesM Unit
script = do
  C.act $ O.Next
  C.act $ O.SetUsername "hellohello"
  C.act $ O.Next
  C.act $ O.SetEmail "nico@hello.de"
  C.act $ O.SetPrivacy true
  C.act $ O.SetTerms true
  C.act $ O.Next

mainAff :: Aff O.State
mainAff = C.exec script O.init

main :: Effect Unit
main = do
  runAff_ (const $ pure unit) mainAff
