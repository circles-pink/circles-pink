module CirclesPink.Garden.StateMachine.State.Login.Views where

import Prelude

import CirclesCore (ApiError, NativeError)
import CirclesPink.Garden.StateMachine.Control.Common (TaskReturn)
import CirclesPink.Garden.StateMachine.Control.EnvControl (UserNotFoundError)
import CirclesPink.Garden.StateMachine.State.Login (LoginState)
import CirclesPink.Garden.StateMachine.ViewUtils (nubRemoteReport)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import CirclesPink.Data.PrivateKey (PrivateKey)
import RemoteReport (RemoteReport)

type DefaultView =
  { magicWords :: String
  , loginResult :: LoginStateLoginResult
  }

type LoginStateLoginResult = RemoteReport ErrLoginStateResolved (TaskReturn /\ PrivateKey)

type ErrLoginStateResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errUserNotFound :: UserNotFoundError
  , errInvalidUrl :: String
  , errSaveSession :: Unit
  , errInvalidMnemonic :: Unit
  , errParseAddress :: String
  )

defaultView :: LoginState -> DefaultView
defaultView s =
  { magicWords: s.magicWords
  , loginResult: nubRemoteReport s.loginResult
  }