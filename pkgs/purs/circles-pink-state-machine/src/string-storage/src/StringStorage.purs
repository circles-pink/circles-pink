module StringStorage where

import Prelude

import Control.Monad.Except (ExceptT(..), lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (hush, note)
import Data.Maybe (Maybe)
import Debug.Extra (todo)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (clear, getItem, removeItem, setItem)

type StringStorage =
  { setItem :: String -> String -> Aff Unit
  , getItem :: String -> Aff (Maybe String)
  , deleteItem :: String -> Aff Unit
  , clear :: Aff Unit
  }

getLocalStorage :: Effect (Maybe StringStorage)
getLocalStorage = runMaybeT do
  w <- try window <#> hush # MaybeT
  s <- localStorage w # liftEffect
  pure
    { setItem: \k v -> liftEffect $ setItem k v s
    , getItem: \k -> todo -- ExceptT $ liftEffect $ note unit <$> getItem k s
    , clear: todo -- liftEffect $ clear s
    , deleteItem: todo -- \k -> liftEffect $ removeItem k s
    }

getSessionStorage :: Effect (Maybe StringStorage)
getSessionStorage = todo