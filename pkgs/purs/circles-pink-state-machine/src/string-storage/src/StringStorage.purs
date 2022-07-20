module StringStorage where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (try)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (clear, getItem, removeItem, setItem)

type StringStorage =
  { setItem :: String -> String -> Aff Unit
  , getItem :: String -> ExceptT Unit Aff String
  , deleteItem :: String -> ExceptT Unit Aff Unit
  , clear :: Aff Unit
  }

getLocalStorage :: Effect (Maybe StringStorage)
getLocalStorage = runMaybeT do
  w <- try window # hush
  s <- localStorage w
  pure
    { setItem: \k v -> setItem k v s
    , getItem: \k -> getItem k s
    , clear: clear s
    , removeItem: \k -> removeItem k s
    }

getSessionStorage :: Effect (Maybe StringStorage)
getSessionStorage = todo