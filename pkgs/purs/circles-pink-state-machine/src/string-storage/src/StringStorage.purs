module StringStorage where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Maybe (Maybe)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff)

type StringStorage =
  { setItem :: String -> String -> Aff Unit
  , getItem :: String -> ExceptT Unit Aff String
  , deleteItem :: String -> ExceptT Unit Aff Unit
  , clear :: Aff Unit
  }

getLocalStorage :: Effect (Maybe StringStorage)
getLocalStorage = todo

getSessionStorage :: Effect (Maybe StringStorage)
getSessionStorage = todo