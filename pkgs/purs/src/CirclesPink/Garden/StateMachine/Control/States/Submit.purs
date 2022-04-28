module CirclesPink.Garden.StateMachine.Control.States.Submit where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, readyForDeployment, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import RemoteData (RemoteData, _failure, _loading, _notAsked)
import Wallet.PrivateKey as P

submit ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { prev :: ActionHandler t m Unit S.UserData ( "magicWords" :: S.UserData )
  , submit :: ActionHandler t m Unit S.UserData ( "submit" :: S.UserData, "trusts" :: S.TrustState )
  }
submit env =
  { prev: \set _ _ -> set \st -> S._magicWords st { direction = D._backwards }
  , submit: submit'
  }
  where
  submit' set st _ = do
    set \st' -> S._submit st' { submitResult = _loading :: RemoteData S.ErrSubmitResolved Unit }
    let
      address = P.privKeyToAddress st.privateKey

      nonce = P.addressToNonce address

      task :: ExceptV S.ErrSubmit _ _
      task = do
        safeAddress <- env.getSafeAddress st.privateKey
        _ <- env.safePrepareDeploy st.privateKey
        env.userRegister
          st.privateKey
          { email: st.email
          , nonce
          , safeAddress
          , username: st.username
          }
        safeStatus <- env.getSafeStatus st.privateKey
        user <- env.userResolve st.privateKey
        trusts <- env.trustGetNetwork st.privateKey
        isReady' <- readyForDeployment env st.privateKey
        pure { safeStatus, user, trusts, isReady: isReady' }
    result <- run' task
    case result of
      Left e -> set \st' -> S._submit st' { submitResult = _failure e }
      Right { safeStatus, user, trusts, isReady } ->
        set \_ ->
          S._trusts
            { user
            , trusts
            , privKey: st.privateKey
            , safeStatus
            , trustsResult: _notAsked
            , isReady
            }
