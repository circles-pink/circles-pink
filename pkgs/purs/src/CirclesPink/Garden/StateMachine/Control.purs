module CirclesPink.Garden.StateMachine.Control (circlesControl) where

import Prelude
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control.Common (readyForDeployment)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Control.States as States
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, catchError, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, default, onMatch)
import Debug (spy)
import Debug.Extra (todo)
import Prim.Row (class Nub)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Wallet.PrivateKey (PrivateKey)
import Wallet.PrivateKey as P

circlesControl ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m -> ((S.CirclesState -> S.CirclesState) -> t m Unit) -> S.CirclesState -> CirclesAction -> t m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { landing: States.landing env
    , infoGeneral:
        { next: \set _ _ -> set \st -> S._askUsername st { direction = D._forwards }
        }
    , askUsername: States.askUsername env
    , askEmail: States.askEmail env
    , infoSecurity: States.infoSecurity env
    , magicWords: States.magicWords env
    , submit: States.submit env
    , dashboard:
        { logout: \_ _ _ -> pure unit
        , getTrusts: dashboardGetTrusts
        }
    , login: States.login env
    , trusts: States.trusts env
    , debug:
        { coreToWindow: debugCoreToWindow
        }
    }
  where
  --------------------------------------------------------------------------------
  -- Debug
  --------------------------------------------------------------------------------
  debugCoreToWindow :: ActionHandler t m Unit S.DebugState ( "debug" :: S.DebugState )
  debugCoreToWindow _ st _ = do
    let
      mnemonic = P.getMnemonicFromString st.magicWords
    let
      privKey = P.mnemonicToKey mnemonic
    _ <- run $ env.coreToWindow privKey
    pure unit

  --------------------------------------------------------------------------------
  -- Dashbaord
  --------------------------------------------------------------------------------
  dashboardGetTrusts :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  dashboardGetTrusts set st _ = do
    result <- run $ env.trustGetNetwork st.privKey
    case result of
      Left e -> set \st' -> S._dashboard st' { error = pure e }
      Right t -> set \st' -> S._dashboard st' { trusts = t }

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v
  = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit

--------------------------------------------------------------------------------
run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

run' :: forall t m e e' a. MonadTrans t => Nub e e' => Monad m => ExceptV e m a -> t m (Either (Variant e') a)
run' = lift <<< runExceptT'
  where
  runExceptT' :: ExceptV e m a -> m (Either (Variant e') a)
  runExceptT' = unsafeCoerce <<< runExceptT
