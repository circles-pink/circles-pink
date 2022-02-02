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

env :: O.Env Aff
env =
  { apiCheckUserName:
      \user -> do
        log $ "checking " <> user
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

reducerAff :: O.Msg -> O.State -> Aff O.State
reducerAff = O.reducer env

mainAff :: Aff O.State
mainAff =
  reducerAff O.Next O.init
    >>= reducerAff (O.SetUsername "hellohello")
    >>= reducerAff O.Next
    >>= reducerAff (O.SetEmail "nico@hello.de")
    >>= reducerAff (O.SetPrivacy true)
    >>= reducerAff (O.SetTerms true)
    >>= reducerAff O.Next

main :: Effect Unit
main = do
  runAff_ (const $ pure unit) mainAff
