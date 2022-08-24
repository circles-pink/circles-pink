module StringStorage where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (try)
import Web.HTML (window)
import Web.HTML.Window (localStorage, sessionStorage)
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
    , getItem: \k -> liftEffect $ getItem k s
    , clear: liftEffect $ clear s
    , deleteItem: \k -> liftEffect $ removeItem k s
    }

getSessionStorage :: Effect (Maybe StringStorage)
getSessionStorage = runMaybeT do
  w <- try window <#> hush # MaybeT
  s <- sessionStorage w # liftEffect
  pure
    { setItem: \k v -> liftEffect $ setItem k v s
    , getItem: \k -> liftEffect $ getItem k s
    , clear: liftEffect $ clear s
    , deleteItem: \k -> liftEffect $ removeItem k s
    }

testStringStorage :: StringStorage
testStringStorage =
  { setItem: \_ _ -> pure unit
  , getItem: \_ -> pure Nothing
  , deleteItem: \_ -> pure unit
  , clear: pure unit
  }