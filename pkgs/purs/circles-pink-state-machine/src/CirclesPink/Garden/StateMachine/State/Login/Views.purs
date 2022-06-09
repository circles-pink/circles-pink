module CirclesPink.Garden.StateMachine.State.Login.Views where

import Prelude

import CirclesCore (ApiError, NativeError)
import CirclesPink.Garden.StateMachine.Control.Common (TaskReturn)
import CirclesPink.Garden.StateMachine.Control.Env (UserNotFoundError)
import CirclesPink.Garden.StateMachine.State.Login (LoginState)
import CirclesPink.Garden.StateMachine.ViewUtils (nubRemoteReport)
import Data.Variant (Variant)
import RemoteReport (RemoteReport)

type DefaultView =
  { magicWords :: String
  , loginResult :: LoginStateLoginResult
  }

type LoginStateLoginResult = RemoteReport ErrLoginStateResolved TaskReturn

type ErrLoginStateResolved = Variant
  ( errApi :: ApiError
  , errNative :: NativeError
  , errUserNotFound :: UserNotFoundError
  , errInvalidUrl :: String
  , errSaveSession :: Unit
  , errInvalidMnemonic :: Unit
  )

defaultView :: LoginState -> DefaultView
defaultView s =
  { magicWords: s.magicWords
  , loginResult: nubRemoteReport s.loginResult
  }