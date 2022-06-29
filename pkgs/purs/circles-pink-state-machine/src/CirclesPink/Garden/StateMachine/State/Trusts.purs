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
import CirclesPink.Garden.StateMachine.Control.Env as Env
import Data.PrivateKey (PrivateKey)
import Data.Variant (Variant, inj)
import RemoteReport (RemoteReport)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------
type ErrTrustState r = Env.ErrGetSafeStatus
  + Env.ErrIsTrusted
  + Env.ErrIsFunded
  + Env.ErrDeploySafe
  + Env.ErrDeployToken
  + r

type TrustStateTrustsResult = RemoteReport
  (Variant (ErrTrustState + ()))
  Unit

type TrustsDeploySafeResult = RemoteReport
  (Variant (Env.ErrDeploySafe + Env.ErrGetSafeStatus + ()))
  SafeStatus

type TrustsDeployTokenResult = RemoteReport
  (Variant (Env.ErrDeployToken + ()))
  String

type TrustState =
  { user :: CC.User
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