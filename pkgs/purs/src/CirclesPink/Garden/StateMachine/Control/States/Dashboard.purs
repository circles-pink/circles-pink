module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  ) where

import Prelude
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler, run, run')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (Either(..), either, isRight)
import Data.String (length)
import Data.Variant (Variant)
import RemoteData (RemoteData, _failure, _loading, _success)
import RemoteReport (RemoteReport)
import Undefined (undefined)
import Wallet.PrivateKey (unsafeAddrFromString)
import Wallet.PrivateKey as P

dashboard ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m ->
  { logout :: ActionHandler t m Unit S.DashboardState ( "landing" :: S.LandingState )
  , getTrusts :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  , addTrustConnection :: ActionHandler t m String S.DashboardState ( "dashboard" :: S.DashboardState )
  , removeTrustConnection :: ActionHandler t m String S.DashboardState ( "dashboard" :: S.DashboardState )
  , getBalance :: ActionHandler t m Unit S.DashboardState ( "dashboard" :: S.DashboardState )
  , transfer ::
      ActionHandler t m
        { from :: String
        , to :: String
        , value :: String
        , paymentNote :: String
        }
        S.DashboardState
        ( "dashboard" :: S.DashboardState )
  , getUsers ::
      ActionHandler t m
        { userNames :: Array String
        , addresses :: Array P.Address
        }
        S.DashboardState
        ( "dashboard" :: S.DashboardState )
  , userSearch ::
      ActionHandler t m
        { query :: String
        }
        S.DashboardState
        ( "dashboard" :: S.DashboardState )
  }
dashboard env =
  { logout: \_ _ _ -> pure unit
  , getTrusts
  , addTrustConnection
  , removeTrustConnection
  , getBalance
  , getUsers
  , transfer
  , userSearch
  }
  where
  getTrusts set st _ = do
    result <-
      run $ env.trustGetNetwork st.privKey
    case result of
      Left e -> set \st' -> S._dashboard st' { error = pure e }
      Right t -> set \st' -> S._dashboard st' { trusts = t }

  addTrustConnection set st u = do
    set \st' -> S._dashboard st' { trustAddResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV S.ErrTrustAddConnection _ _
      task = env.addTrustConnection st.privKey (P.unsafeAddrFromString u) st.user.safeAddress
    result <- run' $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { trustAddResult = _failure e }
      Right _ -> set \st' -> S._dashboard st' { trustAddResult = _success unit }

  removeTrustConnection set st u = do
    set \st' -> S._dashboard st' { trustRemoveResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV S.ErrTrustRemoveConnection _ _
      task = env.removeTrustConnection st.privKey (P.unsafeAddrFromString u) st.user.safeAddress
    result <- run' $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { trustRemoveResult = _failure e }
      Right _ -> set \st' -> S._dashboard st' { trustRemoveResult = _success unit }

  getBalance set st _ = do
    balance <-
      run (env.getBalance st.privKey st.user.safeAddress)
        # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
        # retryUntil env (const { delay: 2000 }) (\r _ -> isRight r) 0
    case balance of
      Left _ -> pure unit
      Right _ -> do
        checkPayout <-
          run (env.checkUBIPayout st.privKey st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
            # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0
        case checkPayout of
          Left _ -> pure unit
          Right checkPayout'
            | checkPayout'.length >= 18 -> do
              _ <-
                run (env.requestUBIPayout st.privKey st.user.safeAddress)
                  # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { requestUBIPayoutResult = r })
                  # retryUntil env (const { delay: 10000 }) (\r n -> n == 5 || isRight r) 0
              pure unit
          _ -> pure unit

  --pure unit
  getUsers set st { userNames, addresses } = do
    set \st' -> S._dashboard st' { getUsersResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV S.ErrGetUsers _ _
      task = env.getUsers st.privKey userNames addresses
    result <- run' $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { getUsersResult = _failure e }
      Right u -> set \st' -> S._dashboard st' { getUsersResult = _success u }

  userSearch set st options = do
    set \st' -> S._dashboard st' { userSearchResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV S.ErrUserSearch _ _
      task = env.userSearch st.privKey options
    result <- run' $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { userSearchResult = _failure e }
      Right u -> set \st' -> S._dashboard st' { userSearchResult = _success u }

  transfer set st { from, to, value, paymentNote } = do
    set \st' -> S._dashboard st' { transferResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV S.ErrTokenTransfer _ _
      task = env.transfer st.privKey (unsafeAddrFromString from) (unsafeAddrFromString to) value paymentNote
    result <- run' $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { transferResult = _failure e }
      Right h -> set \st' -> S._dashboard st' { transferResult = _success h }

--------------------------------------------------------------------------------
type EitherV e a
  = Either (Variant e) a

type RemoteDataV e a
  = RemoteData Unit Unit (Variant e) a

subscribeRemoteData ::
  forall e a t m.
  Monad m => MonadTrans t => Monad (t m) => (RemoteDataV e a -> t m Unit) -> t m (EitherV e a) -> t m (EitherV e a)
subscribeRemoteData setCb comp = do
  setCb $ _loading unit
  result <- comp
  setCb $ either _failure _success result
  pure result

--------------------------------------------------------------------------------
subscribeRemoteReport ::
  forall e a t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m -> (RemoteReport e a -> t m Unit) -> t m (Either e a) -> Int -> t m (Either e a)
subscribeRemoteReport { getTimestamp } setCb comp retry = do
  startTime <- lift getTimestamp
  setCb $ _loading { timestamp: startTime, retry }
  result <- comp
  endTime <- lift getTimestamp
  setCb case result of
    Left e -> _failure { error: e, timestamp: endTime, retry }
    Right d -> _success { data: d, timestamp: endTime, retry }
  pure result

subscribeRemoteReport_ ::
  forall e a t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m -> (RemoteReport e a -> t m Unit) -> t m (Either e a) -> t m (Either e a)
subscribeRemoteReport_ env sub comp = subscribeRemoteReport env sub comp 0

--------------------------------------------------------------------------------
type RetryConfig
  = { delay :: Int
    }

retryUntil ::
  forall t m e a.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env.Env m -> (Int -> RetryConfig) -> (Either e a -> Int -> Boolean) -> Int -> (Int -> t m (Either e a)) -> t m (Either e a)
retryUntil env@{ sleep } getCfg pred retry mkCompu = do
  let
    { delay } = getCfg retry
  result <- mkCompu retry
  let
    newRetry = retry + 1
  lift $ sleep delay
  if pred result retry then
    pure result
  else
    retryUntil env getCfg pred newRetry mkCompu
