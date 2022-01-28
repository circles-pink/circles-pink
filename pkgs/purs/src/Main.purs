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
import Core.State.Onboard (Msg(..))
import Core.State.Onboard as O
import Data.Argonaut.Core (fromObject, fromString, stringifyWithIndent)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..), print)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log, logShow)
import Foreign.Object (fromFoldable)

env :: O.Env Aff
env =
  { apiCheckUserName:
      \u -> do
        log $ "checking " <> u
        r <-
          AX.request
            $ AX.defaultRequest
                { url = "https://api.circles.garden/api/users"
                , method = Left POST
                , responseFormat = ResponseFormat.json
                , content = Just $ Json $ fromObject $ fromFoldable [ "username" /\ fromString u ]
                }
        case r of
          Left e -> do
            log "ERROR"
            log $ printError e
          Right { body } -> log $ stringifyWithIndent 2 body
        log ("API CALL: " <> u)
        pure (u == "foobar")
  }

reducerAff :: O.Msg -> O.State -> Aff O.State
reducerAff = O.reducer env

mainAff :: Aff O.State
mainAff =
  reducerAff O.Next O.init
    >>= reducerAff (O.SetUsername "nico")
    >>= reducerAff O.Next

main :: Effect Unit
main = do
  runAff_ (const $ pure unit) mainAff
