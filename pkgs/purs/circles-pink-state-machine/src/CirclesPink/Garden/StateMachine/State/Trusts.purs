module CirclesPink.Garden.StateMachine.State.Trusts
  ( ErrTrustState
  , TrustState
  , TrustStateTrustsResult
  , TrustsDeploySafeResult
  , TrustsDeployTokenResult
  , _trusts
  ) where

import Prelude

import CirclesCore (SafeStatus, TrustNode)
import CirclesCore as CC
import CirclesPink.Data.PrivateKey (PrivateKey)
import CirclesPink.Data.User (User(..))
import CirclesPink.Garden.StateMachine.Control.EnvControl as EnvControl
import Data.Variant (Variant, inj)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
type ErrTrustState r = EnvControl.ErrGetSafeStatus
  + EnvControl.ErrIsTrusted
  + EnvControl.ErrIsFunded
  + EnvControl.ErrDeploySafe
  + EnvControl.ErrDeployToken
  + r

type TrustStateTrustsResult = RemoteReport
  (Variant (ErrTrustState + ()))
  Unit

type TrustsDeploySafeResult = RemoteReport
  (Variant (EnvControl.ErrDeploySafe + EnvControl.ErrGetSafeStatus + ()))
  SafeStatus

type TrustsDeployTokenResult = RemoteReport
  (Variant (EnvControl.ErrDeployToken + ()))
  String

type TrustState =
  { user :: User
  , privKey :: PrivateKey
  , trusts :: Array TrustNode
  , safeStatus :: SafeStatus
  , isReady :: Boolean
  , trustsResult :: TrustStateTrustsResult
  , deploySafeResult :: TrustsDeploySafeResult
  , deployTokenResult :: TrustsDeployTokenResult
  }

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

_trusts :: forall a v. a -> Variant (trusts :: a | v)
_trusts = inj (Proxy :: _ "trusts")