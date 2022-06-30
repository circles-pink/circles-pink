module CirclesPink.Garden.StateMachine.Control.States.Login where

import Prelude

import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', dropError, loginTask, subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (except, lift, runExceptT)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Variant (inj)
import RemoteData (_loading, _notAsked)
import Type.Proxy (Proxy(..))

login
  :: forall m
   . Monad m
  => Env.Env m
  -> { login :: ActionHandler' m Unit S.LoginState ("trusts" :: S.TrustState, "login" :: S.LoginState, "dashboard" :: S.DashboardState)
     , signUp :: ActionHandler' m Unit S.LoginState ("infoGeneral" :: S.UserData)
     , setMagicWords :: ActionHandler' m String S.LoginState ("login" :: S.LoginState)
     }
login env =
  { login: login'
  , signUp: \set _ _ -> set \_ -> S.init
  , setMagicWords: \set _ words -> set \st -> S._login st { magicWords = words }
  }
  where

  login' set st _ =
    void do
      _ <- set \st' -> S._login st' { loginResult = _loading { previousData: Nothing, retry: 0, timestamp: bottom } }
      runExceptT do
        { safeStatus, user, trusts, isReady } /\ privKey <-
          ( do
              mnemonic <- P.getMnemonicFromString st.magicWords
                # note (inj (Proxy :: _ "errInvalidMnemonic") unit)
                # except

              let privKey = P.mnemonicToKey mnemonic
              taskReturn <- loginTask env privKey
              _ <- env.saveSession privKey
              pure (taskReturn /\ privKey)
          )
            #
              ( \x -> do
                  result <- x
                  _ <- subscribeRemoteReport env (\r -> set \st' -> S._login st' { loginResult = r }) (map fst x) 0
                  pure result
              )
            # dropError
        if safeStatus.isCreated && safeStatus.isDeployed then
          lift $ set \_ -> S.initDashboard { user, privKey }
        else
          lift $ set \_ ->
            S._trusts
              { user
              , trusts
              , privKey
              , safeStatus
              , trustsResult: _notAsked unit
              , deploySafeResult: _notAsked unit
              , deployTokenResult: _notAsked unit
              , isReady
              }

        pure unit

