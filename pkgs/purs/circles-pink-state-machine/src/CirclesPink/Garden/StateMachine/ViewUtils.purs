module CirclesPink.Garden.StateMachine.ViewUtils where

import Data.Variant (Variant)
import Prim.Row (class Nub)
import RemoteReport (RemoteReport)
import Unsafe.Coerce (unsafeCoerce)

nubRemoteReport :: forall e e' a. Nub e e' => RemoteReport (Variant e) a -> RemoteReport (Variant e') a
nubRemoteReport rr = unsafeCoerce rr
