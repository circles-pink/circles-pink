module CirclesPink.Garden.StateMachine.Control.States.MagicWords where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Data.Either (Either(..))

magicWords ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { prev :: ActionHandler t m Unit S.UserData ( "infoSecurity" :: S.UserData )
  , newPrivKey :: ActionHandler t m Unit S.UserData ( "magicWords" :: S.UserData )
  , next :: ActionHandler t m Unit S.UserData ( "submit" :: S.UserData )
  }
magicWords env =
  { prev: \set _ _ -> set \st -> S._infoSecurity st { direction = D._backwards }
  , newPrivKey
  , next: \set _ _ -> set \st -> S._submit st { direction = D._forwards }
  }
  where
  newPrivKey set _ _ = do
    result <- run $ env.generatePrivateKey
    case result of
      Right pk -> set \st -> S._magicWords st { privateKey = pk }
      Left _ -> pure unit
