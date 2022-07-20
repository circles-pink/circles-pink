module CirclesPink.Garden.StateMachine.Control.States.Submit where

import Prelude

import CirclesPink.Data.Nonce (addressToNonce)
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', readyForDeployment, runExceptT')
import CirclesPink.Garden.StateMachine.Control.EnvControl (EnvControl)
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except.Checked (ExceptV)
import Convertable (convert)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Network.Ethereum.Core.Signatures (privateToAddress)
import RemoteData (_failure, _loading, _notAsked)

submit
  :: forall m
   . Monad m
  => EnvControl m
  -> { prev :: ActionHandler' m Unit S.UserData ("magicWords" :: S.UserData)
     , submit :: ActionHandler' m Unit S.UserData ("submit" :: S.UserData, "trusts" :: S.TrustState)
     }
submit env =
  { prev: \set _ _ -> set \st -> S._magicWords st { direction = D._backwards }
  , submit: submit'
  }
  where
  submit' set st _ = do
    set \st' -> S._submit st' { submitResult = _loading unit }
    case st.privateKey of
      Nothing -> pure unit
      Just privateKey -> do
        let
          address = privateToAddress $ unwrap privateKey

          nonce = addressToNonce $ wrap address

          task :: ExceptV S.ErrSubmit _ _
          task = do
            safeAddress <- env.getSafeAddress privateKey
            _ <- env.safePrepareDeploy privateKey
            env.userRegister
              privateKey
              { email: st.email
              , nonce
              , safeAddress: convert safeAddress
              , username: st.username
              }
            safeStatus <- env.getSafeStatus privateKey
            user <- env.userResolve privateKey
            trusts <- env.trustGetNetwork privateKey safeAddress
            isReady' <- readyForDeployment env privateKey
            _ <- env.saveSession privateKey `catchError` (\_ -> pure unit)
            pure { safeStatus, user, trusts, isReady: isReady' }
        result <- runExceptT' task
        case result of
          Left e -> set \st' -> S._submit st' { submitResult = _failure e }
          Right { safeStatus, user, trusts, isReady } ->
            set \_ ->
              S._trusts
                { user
                , trusts
                , privKey: privateKey
                , safeStatus
                , trustsResult: _notAsked unit
                , deploySafeResult: _notAsked unit
                , deployTokenResult: _notAsked unit
                , isReady
                }
