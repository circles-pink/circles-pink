module CirclesPink.Garden.StateMachine.State.Trusts.Views
  ( DefaultView
  , ErrDeploySafeResolved
  , ErrDeployTokenResolved
  , ErrTrustStateResolved
  , defaultView
  ) where

import Prelude

import CirclesCore (NativeError, TrustNode, SafeStatus)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.State (TrustState)
import CirclesPink.Garden.StateMachine.ViewUtils (nubRemoteReport)
import Data.Variant (Variant)
import RemoteReport (RemoteReport)

type DefaultView =
  { user :: CC.User
  , trusts :: Array TrustNode
  , safeStatus :: SafeStatus
  , isReady :: Boolean
  , trustsResult :: RemoteReport ErrTrustStateResolved Unit
  , deploySafeResult :: RemoteReport ErrDeploySafeResolved SafeStatus
  , deployTokenResult :: RemoteReport ErrDeployTokenResolved String
  }

type ErrTrustStateResolved = Variant
  ( errService :: Unit
  , errNative :: NativeError
  , errInvalidUrl :: String
  , errParseAddress :: String
  )

type ErrDeploySafeResolved = Variant
  ( errInvalidUrl :: String
  , errNative :: NativeError
  , errService :: Unit
  , errParseAddress :: String
  )

type ErrDeployTokenResolved = Variant
  ( errService :: Unit
  , errNative :: NativeError
  , errInvalidUrl :: String
  , errParseAddress :: String
  )

defaultView :: TrustState -> DefaultView
defaultView d =
  { user: d.user
  , trusts: d.trusts
  , safeStatus: d.safeStatus
  , isReady: d.isReady
  , trustsResult: nubRemoteReport d.trustsResult
  , deploySafeResult: nubRemoteReport d.deploySafeResult
  , deployTokenResult: nubRemoteReport d.deployTokenResult
  }