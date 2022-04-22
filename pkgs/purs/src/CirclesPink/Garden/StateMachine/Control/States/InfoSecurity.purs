module CirclesPink.Garden.StateMachine.Control.States.InfoSecurity where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..))
import Data.Variant (Variant)
import Wallet.PrivateKey as P

infoSecurity ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { prev :: ((S.UserData -> Variant ( "askEmail" :: S.UserData )) -> t m Unit) -> S.UserData -> Unit -> t m Unit
  , next :: ((S.UserData -> Variant ( "magicWords" :: S.UserData )) -> t m Unit) -> S.UserData -> Unit -> t m Unit
  }
infoSecurity env =
  { prev: \set _ _ -> set \st -> S._askEmail st { direction = D._backwards }
  , next
  }
  where
  --infoSecurityNext :: ActionHandler t m Unit S.UserData ( "magicWords" :: S.UserData )
  next set _ _ = do
    result <- run $ env.generatePrivateKey
    case result of
      Right pk ->
        set \st ->
          if P.zeroKey == st.privateKey then
            S._magicWords st { privateKey = pk, direction = D._forwards }
          else
            S._magicWords st { direction = D._forwards }
      Left _ -> pure unit

--------------------------------------------------------------------------------
run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v
  = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit
